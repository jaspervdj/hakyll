--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module Hakyll.Core.Runtime
    ( run
    , RunMode(..)
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent            (forkIO, getNumCapabilities,
                                                rtsSupportsBoundThreads)
import qualified Control.Concurrent.MVar       as MVar
import           Control.Monad                 (replicateM_, unless, void)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.Trans           (liftIO)
import           Data.Foldable                 (for_, traverse_)
import           Data.IORef                    (IORef)
import qualified Data.IORef                    as IORef
import           Data.List                     (foldl', intercalate)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as Seq
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           System.Exit                   (ExitCode (..))
import           System.FilePath               ((</>))


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

    -- Build runtime read/state
    scheduler <- IORef.newIORef $ emptyScheduler {schedulerFacts = oldFacts}
    let compilers = rulesCompilers ruleSet
        read'     = RuntimeRead
            { runtimeConfiguration = config
            , runtimeLogger        = logger
            , runtimeProvider      = provider
            , runtimeStore         = store
            , runtimeRoutes        = rulesRoutes ruleSet
            , runtimeUniverse      = Map.fromList compilers
            , runtimeScheduler     = scheduler
            }

    -- Run the program and fetch the resulting state
    runReaderT (build mode) read'
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
    , runtimeStore         :: Store
    , runtimeRoutes        :: Routes
    , runtimeUniverse      :: Map Identifier (Compiler SomeItem)
    , runtimeScheduler     :: IORef Scheduler
    }


--------------------------------------------------------------------------------
-- | A Scheduler is a pure representation of work going on, works that needs
-- to be done, and work already done.  Workers can obtain things to do
-- by interacting with the Scheduler, and execute them synchronously or
-- asynchronously.
--
-- All operations on Scheduler look like 'Scheduler -> (Scheduler, a)' and
-- should be used with atomicModifyIORef'.
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
schedulerError :: Maybe Identifier -> String -> Scheduler -> (Scheduler, ())
schedulerError i e s = (s {schedulerErrors = (i, e) : schedulerErrors s}, ())


--------------------------------------------------------------------------------
schedulerMarkOutOfDate
    :: Map Identifier (Compiler SomeItem)
    -> Set Identifier
    -> Scheduler
    -> (Scheduler, [String])
schedulerMarkOutOfDate universe modified scheduler@Scheduler {..} =
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
data SchedulerStep
    -- | The scheduler instructs to offer some work on the given item.  It
    -- also returns the number of threads that can be resumed after they have
    -- starved.
    = SchedulerWork Identifier (Compiler SomeItem) Int
    -- | There's currently no work available, but there will be after other
    -- threads have finished whatever they are doing.
    | SchedulerStarve
    -- | We've finished all work.
    | SchedulerFinish
    -- | An error occurred.  You can retrieve the errors from 'schedulerErrors'.
    | SchedulerError


--------------------------------------------------------------------------------
schedulerPop :: Scheduler -> (Scheduler, SchedulerStep)
schedulerPop scheduler@Scheduler {..} = case Seq.viewl schedulerQueue of
    Seq.EmptyL
        | not $ Set.null schedulerWorking ->
            ( scheduler {schedulerStarved = schedulerStarved + 1}
            , SchedulerStarve
            )
        | not $ Set.null schedulerBlocked ->
            let msg = "Possible dependency cycle in: " <>
                    intercalate ", " (show <$> Set.toList schedulerBlocked) in
            SchedulerError <$ schedulerError Nothing msg scheduler
        | otherwise -> (scheduler, SchedulerFinish)
    x Seq.:< xs
        | x `Set.member` schedulerDone ->
            schedulerPop scheduler {schedulerQueue = xs}
        | x `Set.member` schedulerWorking ->
            schedulerPop scheduler {schedulerQueue = xs}
        | x `Set.member` schedulerBlocked ->
            schedulerPop scheduler {schedulerQueue = xs}
        | otherwise -> case Map.lookup x schedulerTodo of
            Nothing -> SchedulerError <$
                schedulerError (Just x) "Compiler not found" scheduler
            Just c  ->
                ( scheduler
                    { schedulerQueue   = xs
                    , schedulerWorking = Set.insert x schedulerWorking
                    }
                , SchedulerWork x c 0
                )


--------------------------------------------------------------------------------
schedulerBlock
    :: Identifier
    -> [(Identifier, Snapshot)]
    -> Compiler SomeItem
    -> Scheduler
    -> (Scheduler, SchedulerStep)
schedulerBlock identifier deps0 compiler scheduler@Scheduler {..}
    | null deps1 = (scheduler, SchedulerWork identifier compiler 0)
    | otherwise  = schedulerPop $ scheduler
         { schedulerQueue    =
             -- Optimization: move deps to the front and item to the back
             Seq.fromList depIds <>
             schedulerQueue <>
             Seq.singleton identifier
         , schedulerTodo     =
             Map.insert identifier
                 (Compiler $ \_ -> pure $ CompilerRequire deps0 compiler)
                 schedulerTodo
         , schedulerWorking  = Set.delete identifier schedulerWorking
         , schedulerBlocked  = Set.insert identifier schedulerBlocked
         , schedulerTriggers = foldl'
             (\acc (depId, _) ->
                 Map.insertWith Set.union depId (Set.singleton identifier) acc)
             schedulerTriggers
             deps1
         }
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
        { schedulerQueue    =
            schedulerQueue <> Seq.fromList (Set.toList triggered)
        , schedulerStarved  = 0
        , schedulerBlocked  = Set.delete identifier $
            schedulerBlocked `Set.difference` triggered
        , schedulerTriggers = Map.delete identifier schedulerTriggers
        }
    , schedulerStarved
    )
  where
    triggered = fromMaybe Set.empty $ Map.lookup identifier schedulerTriggers


--------------------------------------------------------------------------------
schedulerSnapshot
    :: Identifier -> Snapshot -> Compiler SomeItem
    -> Scheduler -> (Scheduler, SchedulerStep)
schedulerSnapshot identifier snapshot compiler scheduler@Scheduler {..} =
    let (scheduler', resume) = schedulerUnblock identifier scheduler
            { schedulerSnapshots =
                Set.insert (identifier, snapshot) schedulerSnapshots
            } in
    (scheduler', SchedulerWork identifier compiler resume)


--------------------------------------------------------------------------------
schedulerWrite
    :: Identifier
    -> [Dependency]
    -> Scheduler
    -> (Scheduler, SchedulerStep)
schedulerWrite identifier depFacts scheduler0@Scheduler {..} =
    let (scheduler1, resume) = schedulerUnblock identifier scheduler0
            { schedulerWorking = Set.delete identifier schedulerWorking
            , schedulerFacts   = Map.insert identifier depFacts schedulerFacts
            , schedulerDone    =
                Set.insert identifier schedulerDone
            , schedulerTodo    =
                Map.delete identifier schedulerTodo
            }
        (scheduler2, step) = schedulerPop scheduler1 in
    case step of
        SchedulerWork i c n -> (scheduler2, SchedulerWork i c (n + resume))
        _                   -> (scheduler2, step)


--------------------------------------------------------------------------------
build :: RunMode -> ReaderT RuntimeRead IO ()
build mode = do
    logger <- runtimeLogger <$> ask
    Logger.header logger "Checking for out-of-date items"
    schedulerRef <- runtimeScheduler <$> ask
    scheduleOutOfDate
    case mode of
        RunModeNormal -> do
            Logger.header logger "Compiling"
            if rtsSupportsBoundThreads then pickAndChaseAsync else pickAndChase
            Logger.header logger "Success"
            facts <- liftIO $ schedulerFacts <$> IORef.readIORef schedulerRef
            store <- runtimeStore <$> ask
            liftIO $ Store.set store factsKey facts
        RunModePrintOutOfDate -> do
            Logger.header logger "Out of date items:"
            todo <- liftIO $ schedulerTodo <$> IORef.readIORef schedulerRef
            traverse_ (Logger.message logger . show) (Map.keys todo)


--------------------------------------------------------------------------------
scheduleOutOfDate :: ReaderT RuntimeRead IO ()
scheduleOutOfDate = do
    logger       <- runtimeLogger    <$> ask
    provider     <- runtimeProvider  <$> ask
    universe     <- runtimeUniverse  <$> ask
    schedulerRef <- runtimeScheduler <$> ask
    let modified  = Set.filter (resourceModified provider) (Map.keysSet universe)
    msgs <- liftIO . IORef.atomicModifyIORef' schedulerRef $
        schedulerMarkOutOfDate universe modified

    -- Print messages
    mapM_ (Logger.debug logger) msgs


--------------------------------------------------------------------------------
pickAndChase :: ReaderT RuntimeRead IO ()
pickAndChase = do
    scheduler <- runtimeScheduler <$> ask
    let go SchedulerFinish       = pure ()
        go SchedulerError        = pure ()
        go (SchedulerWork i c _) = work i c >>= go
        go SchedulerStarve       =
            liftIO . IORef.atomicModifyIORef' scheduler $
            schedulerError Nothing "Starved, possible dependency cycle?"
    pop <- liftIO . IORef.atomicModifyIORef' scheduler $ schedulerPop
    go pop


--------------------------------------------------------------------------------
pickAndChaseAsync :: ReaderT RuntimeRead IO ()
pickAndChaseAsync = do
    runtimeRead <- ask
    numThreads  <- liftIO getNumCapabilities
    let scheduler = runtimeScheduler runtimeRead
    Logger.message (runtimeLogger runtimeRead) $
        "Using async runtime with " <> show numThreads <> " threads..."
    liftIO $ do
        signal     <- MVar.newEmptyMVar

        let spawnN :: Int -> IO ()
            spawnN n = replicateM_ n $ forkIO $
                IORef.atomicModifyIORef' scheduler schedulerPop >>= go

            go :: SchedulerStep -> IO ()
            go step = case step of
                SchedulerFinish       -> void $ MVar.tryPutMVar signal ()
                SchedulerStarve       -> pure ()
                SchedulerError        -> void $ MVar.tryPutMVar signal ()
                (SchedulerWork i c n) -> do
                    spawnN n
                    step' <- runReaderT (work i c) runtimeRead
                    go step'

        spawnN numThreads
        MVar.readMVar signal


--------------------------------------------------------------------------------
work :: Identifier -> Compiler SomeItem -> ReaderT RuntimeRead IO SchedulerStep
work id' compiler = do
    logger    <- runtimeLogger        <$> ask
    provider  <- runtimeProvider      <$> ask
    universe  <- runtimeUniverse      <$> ask
    routes    <- runtimeRoutes        <$> ask
    store     <- runtimeStore         <$> ask
    config    <- runtimeConfiguration <$> ask
    scheduler <- runtimeScheduler     <$> ask

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
            for_ msgs $ \msg -> liftIO . IORef.atomicModifyIORef' scheduler $
                schedulerError (Just id') msg
            return SchedulerError

        CompilerSnapshot snapshot c -> do
            liftIO . IORef.atomicModifyIORef' scheduler $
                schedulerSnapshot id' snapshot c

        CompilerDone (SomeItem item) cwrite -> do
            -- Print some info
            let facts = compilerDependencies cwrite
                cacheHits
                    | compilerCacheHits cwrite <= 0 = "updated"
                    | otherwise                     = "cached "
            Logger.message logger $ cacheHits ++ " " ++ show id'

            -- Sanity check
            liftIO . unless (itemIdentifier item == id') $
                IORef.atomicModifyIORef' scheduler $ schedulerError
                    (Just id') $
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

            liftIO $ save store item
            liftIO . IORef.atomicModifyIORef' scheduler $
                schedulerWrite id' facts

        CompilerRequire reqs c -> do
            liftIO . IORef.atomicModifyIORef' scheduler $
                schedulerBlock id' reqs c
