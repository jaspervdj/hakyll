--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module Hakyll.Core.Runtime
    ( run
    , RunMode(..)
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.Async.Lifted (forConcurrently)
import           Control.Concurrent.MVar         (modifyMVar_, readMVar, newMVar, MVar)
import           Control.Monad                   (join, unless, when)
import           Control.Monad.Except            (ExceptT, runExceptT, throwError)
import           Control.Monad.Reader            (ReaderT, ask, runReaderT)
import           Control.Monad.Trans             (liftIO)
import           Data.Foldable                   (for_, traverse_)
import           Data.List                       (foldl')
import           Data.IORef                      (IORef)
import qualified Data.IORef                      as IORef
import           Data.List                       (intercalate)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Sequence                   (Seq)
import qualified Data.Sequence                   as Seq
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Traversable                (for)
import           Debug.Trace                     (trace)
import           System.Exit                     (ExitCode (..))
import           System.FilePath                 ((</>))


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Compiler.Require
import           Hakyll.Core.Configuration
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Item.SomeItem
import           Hakyll.Core.Logger            (Logger)
import qualified Hakyll.Core.Logger            as Logger
import           Hakyll.Core.Provider
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Store             (Store)
import qualified Hakyll.Core.Store             as Store
import           Hakyll.Core.Util.File
import           Hakyll.Core.Writable


factsKey :: [String]
factsKey = ["Hakyll.Core.Runtime.run", "facts"]


--------------------------------------------------------------------------------
-- | Whether to execute a normal run (build the site) or a dry run.
data RunMode = RunModeNormal | RunModePrintOutOfDate
    deriving (Show)


--------------------------------------------------------------------------------
run :: RunMode -> Configuration -> Logger -> Rules a -> IO (ExitCode, RuleSet)
run mode config logger rules = do
    -- Initialization
    Logger.header logger "Initialising..."
    Logger.message logger "Creating store..."
    store <- Store.new (inMemoryCache config) $ storeDirectory config
    Logger.message logger "Creating provider..."
    provider <- newProvider store (shouldIgnoreFile config) $
        providerDirectory config
    Logger.message logger "Running rules..."
    ruleSet  <- runRules rules provider

    -- Get old facts
    mOldFacts <- Store.get store factsKey
    let (oldFacts) = case mOldFacts of Store.Found f -> f
                                       _             -> mempty

    state <- newMVar $ RuntimeState
            { runtimeDone         = Set.empty
            , runtimeSnapshots    = Set.empty
            , runtimeTodo         = Map.empty
            , runtimeFacts        = oldFacts
            , runtimeDependencies = Map.empty
            }

    -- Build runtime read/state
    scheduler <- IORef.newIORef $ emptyScheduler {schedulerFacts = oldFacts}
    let compilers = rulesCompilers ruleSet
        read'     = RuntimeRead
            { runtimeConfiguration = config
            , runtimeLogger        = logger
            , runtimeProvider      = provider
            , runtimeState         = state
            , runtimeStore         = store
            , runtimeRoutes        = rulesRoutes ruleSet
            , runtimeUniverse      = Map.fromList compilers
            , runtimeScheduler     = scheduler
            }

    -- Run the program and fetch the resulting state
    result <- runReaderT (build2 mode) read'
    errors <- schedulerErrors <$> IORef.readIORef scheduler
    if null errors then do
        Logger.debug logger "Removing tmp directory..."
        removeDirectory $ tmpDirectory config

        Logger.flush logger
        return (ExitSuccess, ruleSet)
    else do
        for_ errors $ \(mbId, err) -> Logger.error logger $ case mbId of
            Just identifier -> show identifier <> ": " <> err
            Nothing         -> err
        Logger.flush logger
        return (ExitFailure 1, ruleSet)


--------------------------------------------------------------------------------
data RuntimeRead = RuntimeRead
    { runtimeConfiguration :: Configuration
    , runtimeLogger        :: Logger
    , runtimeProvider      :: Provider
    , runtimeState         :: MVar RuntimeState
    , runtimeStore         :: Store
    , runtimeRoutes        :: Routes
    , runtimeUniverse      :: Map Identifier (Compiler SomeItem)
    , runtimeScheduler     :: IORef Scheduler
    }


--------------------------------------------------------------------------------
data RuntimeState = RuntimeState
    { runtimeDone         :: Set Identifier
    , runtimeSnapshots    :: Set (Identifier, Snapshot)
    , runtimeTodo         :: Map Identifier (Compiler SomeItem)
    , runtimeFacts        :: DependencyFacts
    , runtimeDependencies :: Map Identifier (Set (Identifier, Snapshot))
    }


--------------------------------------------------------------------------------
data Scheduler = Scheduler
    { -- | Items to work on next.  Identifiers may appear multiple times.
      schedulerQueue     :: !(Seq Identifier)
    , -- | Items that we haven't started yet.
      schedulerTodo      :: !(Map Identifier (Compiler SomeItem))
    , -- | Currently processing
      schedulerWorking   :: !(Set Identifier)
    , -- | Finished
      schedulerDone      :: !(Set Identifier)
    , -- | Any snapshots stored.
      schedulerSnapshots :: !(Set (Identifier, Snapshot))
    , -- | Currently blocked compilers.
      schedulerBlocked   :: !(Set Identifier)
    , -- | Compilers that may resume on triggers
      schedulerTriggers  :: !(Map Identifier (Set Identifier))
    , -- | Number of starved pops; tracking this allows us to start a new
      -- number of threads again later.
      schedulerStarved   :: !Int
    , -- | Dynamic dependency info.
      schedulerFacts     :: !DependencyFacts
    , -- | Errors encountered.
      schedulerErrors    :: ![(Maybe Identifier, String)]
    }


--------------------------------------------------------------------------------
emptyScheduler :: Scheduler
emptyScheduler = Scheduler {..}
  where
    schedulerTodo      = Map.empty
    schedulerDone      = Set.empty
    schedulerQueue     = Seq.empty
    schedulerWorking   = Set.empty
    schedulerSnapshots = Set.empty
    schedulerBlocked   = Set.empty
    schedulerTriggers  = Map.empty
    schedulerStarved   = 0
    schedulerFacts     = Map.empty
    schedulerErrors    = []


--------------------------------------------------------------------------------
schedulerOutOfDate
    :: Map Identifier (Compiler SomeItem)
    -> Set Identifier
    -> Scheduler
    -> (Scheduler, [String])
schedulerOutOfDate universe modified scheduler@Scheduler {..} =
    ( scheduler
        { schedulerQueue = schedulerQueue <> Seq.fromList (Map.keys todo)
        , schedulerDone  = schedulerDone <>
            (Map.keysSet universe `Set.difference` ood)
        , schedulerTodo  = schedulerTodo <> todo
        , schedulerFacts = facts'
        }
    , msgs
    )
  where
    (ood, facts', msgs) = outOfDate (Map.keys universe) modified schedulerFacts
    todo = Map.filterWithKey (\id' _ -> id' `Set.member` ood) universe


--------------------------------------------------------------------------------
data Pop
    = PopOk Identifier (Compiler SomeItem)
    | PopError
    | PopFinished
    | PopStarve


--------------------------------------------------------------------------------
schedulerPop :: Scheduler -> (Scheduler, Pop)
schedulerPop scheduler@Scheduler {..} = case Seq.viewl schedulerQueue of
    Seq.EmptyL
        | Set.null schedulerWorking -> (scheduler, PopFinished)
        | otherwise ->
            (scheduler {schedulerStarved = schedulerStarved + 1}, PopStarve)
    x Seq.:< xs
        | x `Set.member` schedulerDone ->
            schedulerPop scheduler {schedulerQueue = xs}
        | x `Set.member` schedulerWorking ->
            schedulerPop scheduler {schedulerQueue = xs}
        | x `Set.member` schedulerBlocked ->
            schedulerPop scheduler {schedulerQueue = xs}
        | otherwise -> case Map.lookup x schedulerTodo of
            Nothing ->
                ( scheduler
                    { schedulerErrors = (Just x, "Compiler not found") : schedulerErrors
                    }
                , PopError
                )
            Just c  ->
                ( scheduler
                    { schedulerQueue   = xs
                    , schedulerWorking = Set.insert x schedulerWorking
                    }
                , PopOk x c
                )


--------------------------------------------------------------------------------
data Block
    = BlockContinue
    | BlockBlocked


--------------------------------------------------------------------------------
schedulerBlock
    :: Identifier
    -> [(Identifier, Snapshot)]
    -> Compiler SomeItem
    -> Scheduler
    -> (Scheduler, Block)
schedulerBlock identifier deps0 compiler scheduler@Scheduler {..}
    | null deps1 =
        trace ("done for identifier " <> show identifier <> ", " <> show deps1 <> ", " <> show deps0) $
        (scheduler, BlockContinue)
    | otherwise      =
        ( scheduler
             { schedulerQueue    =
                 -- Optimization: move deps to the front and item to the back
                 Seq.fromList depIds <>
                 schedulerQueue <>
                 Seq.singleton identifier
             , schedulerTodo     =
                 trace ("insert for identifier " <> show identifier) $
                 Map.insert identifier compiler schedulerTodo
             , schedulerWorking  = Set.delete identifier schedulerWorking
             , schedulerBlocked  = Set.insert identifier schedulerBlocked
             , schedulerTriggers = foldl'
                 (\acc (depId, _) ->
                     Map.insertWith Set.union depId (Set.singleton identifier) acc)
                 schedulerTriggers
                 deps1
             }
        , BlockBlocked
        )
  where
    deps1  = filter (not . done) deps0
    depIds = map fst deps1

    -- Done if we either completed the entire item (runtimeDone) or
    -- if we previously saved the snapshot (runtimeSnapshots).
    done (depId, depSnapshot) =
        depId `Set.member` schedulerDone ||
        (depId, depSnapshot) `Set.member` schedulerSnapshots


--------------------------------------------------------------------------------
schedulerUnblock :: Identifier -> Scheduler -> (Scheduler, Int)
schedulerUnblock identifier scheduler@Scheduler {..} =
    ( scheduler
        { schedulerQueue    = schedulerQueue <>
            Seq.fromList (Set.toList triggered)
        , schedulerStarved  = 0
        , schedulerBlocked  = schedulerBlocked `Set.difference` triggered
        , schedulerTriggers = Map.delete identifier schedulerTriggers
        }
    , schedulerStarved
    )
  where
    triggered = fromMaybe Set.empty $ Map.lookup identifier schedulerTriggers


--------------------------------------------------------------------------------
schedulerSnapshot
    :: Identifier -> Snapshot -> Compiler SomeItem -> Scheduler -> (Scheduler, Int)
schedulerSnapshot identifier snapshot compiler scheduler@Scheduler {..} =
    schedulerUnblock identifier $ scheduler
        { schedulerQueue     = Seq.singleton identifier <> schedulerQueue
        , schedulerWorking   = Set.delete identifier schedulerWorking
        , schedulerSnapshots = Set.insert (identifier, snapshot) schedulerSnapshots
        , schedulerTodo      =
            trace ("snapshot for identifier " <> show identifier <> " " <> snapshot) $
            Map.insert identifier compiler schedulerTodo
        }


--------------------------------------------------------------------------------
schedulerWrite
    :: Identifier
    -> [Dependency]
    -> Scheduler
    -> (Scheduler, Int)
schedulerWrite identifier depFacts scheduler@Scheduler {..} =
    schedulerUnblock identifier $ scheduler
        { schedulerWorking = Set.delete identifier schedulerWorking
        , schedulerFacts   = Map.insert identifier depFacts schedulerFacts
        , schedulerDone    =
            trace ("write for identifier " <> show identifier) $
            Set.insert identifier schedulerDone
        , schedulerTodo    =
            trace ("delete for identifier " <> show identifier) $
            Map.delete identifier schedulerTodo
        }


--------------------------------------------------------------------------------
type Runtime a = ReaderT RuntimeRead (ExceptT String IO) a


--------------------------------------------------------------------------------
-- Because compilation of rules often revolves around IO,
-- be very careful when modifying the state
modifyRuntimeState :: (RuntimeState -> RuntimeState) -> Runtime ()
modifyRuntimeState f = liftIO . flip modifyMVar_ (pure . f) . runtimeState =<< ask


--------------------------------------------------------------------------------
getRuntimeState :: Runtime RuntimeState
getRuntimeState = liftIO . readMVar . runtimeState =<< ask


--------------------------------------------------------------------------------
build :: RunMode -> Runtime ()
build mode = do
    logger <- runtimeLogger <$> ask
    Logger.header logger "Checking for out-of-date items"
    scheduleOutOfDate
    case mode of
        RunModeNormal -> do
            Logger.header logger "Compiling"
            pickAndChase
            Logger.header logger "Success"
            facts <- runtimeFacts <$> getRuntimeState
            store <- runtimeStore <$> ask
            liftIO $ Store.set store factsKey facts
        RunModePrintOutOfDate -> do
            Logger.header logger "Out of date items:"
            todo <- runtimeTodo <$> getRuntimeState
            traverse_ (Logger.message logger . show) (Map.keys todo)


--------------------------------------------------------------------------------
build2 :: RunMode -> ReaderT RuntimeRead IO ()
build2 mode = do
    logger <- runtimeLogger <$> ask
    Logger.header logger "Checking for out-of-date items"
    schedulerRef <- runtimeScheduler <$> ask
    scheduleOutOfDate2
    case mode of
        RunModeNormal -> do
            Logger.header logger "Compiling"
            pickAndChase2
            Logger.header logger "Success"
            facts <- liftIO $ schedulerFacts <$> IORef.readIORef schedulerRef
            store <- runtimeStore <$> ask
            liftIO $ Store.set store factsKey facts
        RunModePrintOutOfDate -> do
            Logger.header logger "Out of date items:"
            todo <- liftIO $ schedulerTodo <$> IORef.readIORef schedulerRef
            traverse_ (Logger.message logger . show) (Map.keys todo)


--------------------------------------------------------------------------------
scheduleOutOfDate :: Runtime ()
scheduleOutOfDate = do
    logger   <- runtimeLogger   <$> ask
    provider <- runtimeProvider <$> ask
    universe <- runtimeUniverse <$> ask

    let identifiers = Map.keys universe
        modified    = Set.filter (resourceModified provider) (Map.keysSet universe)

    state <- getRuntimeState
    let facts = runtimeFacts state
        todo  = runtimeTodo state
        done  = runtimeDone state

    let (ood, facts', msgs) = outOfDate identifiers modified facts
        todo'               = Map.filterWithKey (\id' _ -> id' `Set.member` ood) universe
        done'               = done `Set.union` (Map.keysSet universe `Set.difference` ood)

    -- Print messages
    mapM_ (Logger.debug logger) msgs

    -- Update facts and todo items
    modifyRuntimeState $ \s -> s
        { runtimeDone  = done'
        , runtimeTodo  = todo `Map.union` todo'
        , runtimeFacts = facts'
        }


--------------------------------------------------------------------------------
scheduleOutOfDate2 :: ReaderT RuntimeRead IO ()
scheduleOutOfDate2 = do
    logger       <- runtimeLogger   <$> ask
    provider     <- runtimeProvider <$> ask
    universe     <- runtimeUniverse <$> ask
    schedulerRef <- runtimeScheduler <$> ask
    let modified  = Set.filter (resourceModified provider) (Map.keysSet universe)
    msgs <- liftIO . IORef.atomicModifyIORef' schedulerRef $
        schedulerOutOfDate universe modified

    -- Print messages
    mapM_ (Logger.debug logger) msgs


--------------------------------------------------------------------------------
pickAndChase :: Runtime ()
pickAndChase = do
    todo <- runtimeTodo <$> getRuntimeState
    unless (null todo) $ do
        acted <- mconcat <$> forConcurrently (Map.keys todo) chase
        when (acted == Idled) $ do
            -- This clause happens when chasing *every item* in `todo` resulted in 
            -- idling because tasks are all waiting on something: a dependency cycle  
            deps <- runtimeDependencies <$> getRuntimeState
            throwError $ "Hakyll.Core.Runtime.pickAndChase: Dependency cycle detected: " ++ 
                intercalate ", " [show k ++ " depends on " ++ show (Set.toList v) | (k, v) <- Map.toList deps]
        pickAndChase


--------------------------------------------------------------------------------
pickAndChase2 :: ReaderT RuntimeRead IO ()
pickAndChase2 = do
    (continue, _) <- chase2
    when continue pickAndChase2


--------------------------------------------------------------------------------
chase2 :: ReaderT RuntimeRead IO (Bool, Int)
chase2 = do
    logger    <- runtimeLogger        <$> ask
    provider  <- runtimeProvider      <$> ask
    universe  <- runtimeUniverse      <$> ask
    routes    <- runtimeRoutes        <$> ask
    store     <- runtimeStore         <$> ask
    config    <- runtimeConfiguration <$> ask
    scheduler <- runtimeScheduler <$> ask

    let addError mbId err = liftIO . IORef.atomicModifyIORef' scheduler $ \s ->
            ( s {schedulerErrors = (mbId, err) : schedulerErrors s}
            , ()
            )

    pop <- liftIO . IORef.atomicModifyIORef' scheduler $ schedulerPop
    case pop of
        PopError -> pure (False, 0)
        PopFinished -> pure (False, 0)
        PopStarve -> pure (False, 0)
        PopOk id' compiler -> do
            let cread = CompilerRead
                    { compilerConfig     = config
                    , compilerUnderlying = id'
                    , compilerProvider   = provider
                    , compilerUniverse   = Map.keysSet universe
                    , compilerRoutes     = routes
                    , compilerStore      = store
                    , compilerLogger     = logger
                    }
            result <- liftIO $ runCompiler compiler cread
            case result of
                CompilerError e -> do
                    let msgs = case compilerErrorMessages e of
                            [] -> ["Compiler failed but no info given, try running with -v?"]
                            es -> es
                    for_ msgs . addError $ Just id'
                    return (False, 0)

                CompilerSnapshot snapshot c -> do
                    threads <- liftIO . IORef.atomicModifyIORef' scheduler $
                        schedulerSnapshot id' snapshot c
                    pure (True, threads)

                CompilerDone (SomeItem item) cwrite -> do
                    -- Print some info
                    let facts = compilerDependencies cwrite
                        cacheHits
                            | compilerCacheHits cwrite <= 0 = "updated"
                            | otherwise                     = "cached "
                    Logger.message logger $ cacheHits ++ " " ++ show id'

                    -- Sanity check
                    unless (itemIdentifier item == id') $ addError (Just id') $
                        "The compiler yielded an Item with Identifier " ++
                        show (itemIdentifier item) ++ ", but we were expecting " ++
                        "an Item with Identifier " ++ show id' ++ " " ++
                        "(you probably want to call makeItem to solve this problem)"

                    -- Write if necessary
                    (mroute, _) <- liftIO $ runRoutes routes provider id'
                    case mroute of
                        Nothing    -> return ()
                        Just route -> do
                            let path = destinationDirectory config </> route
                            liftIO $ makeDirectories path
                            liftIO $ write path item
                            Logger.debug logger $ "Routed to " ++ path

                    Logger.message logger $ "Saved _final for " <> show id'
                    liftIO $ save store item
                    threads <- liftIO . IORef.atomicModifyIORef' scheduler $
                        schedulerWrite id' facts
                    pure (True, threads)

                CompilerRequire reqs c -> do
                    block <- liftIO . IORef.atomicModifyIORef' scheduler $
                        schedulerBlock id' reqs $ Compiler $
                        \_ -> pure $ CompilerRequire reqs c
                    case block of
                        BlockContinue -> pure (True, 0)
                        BlockBlocked -> pure (True, 0)


--------------------------------------------------------------------------------
-- | Tracks whether a set of tasks has progressed overall (at least one task progressed)
-- or has idled
data Progress = Progressed | Idled deriving (Eq)

instance Semigroup Progress where
    Idled      <> Idled      = Idled
    Progressed <> _          = Progressed
    _          <> Progressed = Progressed

instance Monoid Progress where
    mempty = Idled


--------------------------------------------------------------------------------
chase :: Identifier -> Runtime Progress
chase id' = do
    logger    <- runtimeLogger        <$> ask
    provider  <- runtimeProvider      <$> ask
    universe  <- runtimeUniverse      <$> ask
    routes    <- runtimeRoutes        <$> ask
    store     <- runtimeStore         <$> ask
    config    <- runtimeConfiguration <$> ask

    state     <- getRuntimeState

    Logger.debug logger $ "Processing " ++ show id'

    let compiler = (runtimeTodo state) Map.! id'
        read' = CompilerRead
            { compilerConfig     = config
            , compilerUnderlying = id'
            , compilerProvider   = provider
            , compilerUniverse   = Map.keysSet universe
            , compilerRoutes     = routes
            , compilerStore      = store
            , compilerLogger     = logger
            }

    result <- liftIO $ runCompiler compiler read'
    case result of
        -- Rethrow error
        CompilerError e -> throwError $ case compilerErrorMessages e of
            [] -> "Compiler failed but no info given, try running with -v?"
            es -> intercalate "; " es

        -- Signal that a snapshot was saved ->
        CompilerSnapshot snapshot c -> do
            -- Update info. The next 'chase' will pick us again at some
            -- point so we can continue then.
            modifyRuntimeState $ \s -> s
                { runtimeSnapshots = Set.insert (id', snapshot) (runtimeSnapshots s)
                , runtimeTodo      = Map.insert id' c (runtimeTodo s)
                }

            return Progressed


        -- Huge success
        CompilerDone (SomeItem item) cwrite -> do
            -- Print some info
            let facts = compilerDependencies cwrite
                cacheHits
                    | compilerCacheHits cwrite <= 0 = "updated"
                    | otherwise                     = "cached "
            Logger.message logger $ cacheHits ++ " " ++ show id'

            -- Sanity check
            unless (itemIdentifier item == id') $ throwError $
                "The compiler yielded an Item with Identifier " ++
                show (itemIdentifier item) ++ ", but we were expecting " ++
                "an Item with Identifier " ++ show id' ++ " " ++
                "(you probably want to call makeItem to solve this problem)"

            -- Write if necessary
            (mroute, _) <- liftIO $ runRoutes routes provider id'
            case mroute of
                Nothing    -> return ()
                Just route -> do
                    let path = destinationDirectory config </> route
                    liftIO $ makeDirectories path
                    liftIO $ write path item
                    Logger.debug logger $ "Routed to " ++ path

            -- Save! (For load)
            liftIO $ save store item

            modifyRuntimeState $ \s -> s
                { runtimeDone         = Set.insert id' (runtimeDone s)
                , runtimeTodo         = Map.delete id' (runtimeTodo s)
                , runtimeFacts        = Map.insert id' facts (runtimeFacts s)
                , runtimeDependencies = Map.delete id' (runtimeDependencies s)
                }

            return Progressed

        -- Try something else first
        CompilerRequire reqs c -> do
            let done      = runtimeDone state
                snapshots = runtimeSnapshots state

            deps <- fmap join . for reqs $ \(depId, depSnapshot) -> do
                Logger.debug logger $
                    "Compiler requirement found for: " ++ show id' ++
                    ": " ++ show depId ++ " (snapshot " ++ depSnapshot ++ ")"

                -- Done if we either completed the entire item (runtimeDone) or
                -- if we previously saved the snapshot (runtimeSnapshots).
                let depDone =
                        depId `Set.member` done ||
                        (depId, depSnapshot) `Set.member` snapshots
                    actualDep = [(depId, depSnapshot) | not depDone]

                return actualDep  

            modifyRuntimeState $ \s -> s
                { runtimeTodo         = Map.insert id'
                    (if null deps then c else compilerResult result)
                    (runtimeTodo s)
                 -- We track dependencies only to inform users when an infinite loop is detected
                , runtimeDependencies = Map.insertWith Set.union id' (Set.fromList deps) (runtimeDependencies s)
                }

            -- Progress has been made if at least one of the 
            -- requirements can move forwards at the next pass
            -- In some cases, dependencies have been processed in parallel in which case `deps` 
            -- can be empty, and we can progress to the next stage. See issue #907
            let progress | null deps    = Progressed
                         | deps == reqs = Idled
                         | otherwise    = Progressed

            return progress
