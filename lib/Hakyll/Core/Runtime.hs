--------------------------------------------------------------------------------
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
import           Data.Foldable                   (traverse_)
import           Data.List                       (intercalate)
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Set                        (Set)
import qualified Data.Set                        as S
import           Data.Traversable                (for)
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
            { runtimeDone         = S.empty
            , runtimeSnapshots    = S.empty
            , runtimeTodo         = M.empty
            , runtimeFacts        = oldFacts
            , runtimeDependencies = M.empty
            }

    -- Build runtime read/state
    let compilers = rulesCompilers ruleSet
        read'     = RuntimeRead
            { runtimeConfiguration = config
            , runtimeLogger        = logger
            , runtimeProvider      = provider
            , runtimeState         = state
            , runtimeStore         = store
            , runtimeRoutes        = rulesRoutes ruleSet
            , runtimeUniverse      = M.fromList compilers
            }

    -- Run the program and fetch the resulting state
    result <- runExceptT $ runReaderT (build mode) read'
    case result of
        Left e          -> do
            Logger.error logger e
            Logger.flush logger
            return (ExitFailure 1, ruleSet)

        Right _ -> do
            Logger.debug logger "Removing tmp directory..."
            removeDirectory $ tmpDirectory config

            Logger.flush logger
            return (ExitSuccess, ruleSet)


--------------------------------------------------------------------------------
data RuntimeRead = RuntimeRead
    { runtimeConfiguration :: Configuration
    , runtimeLogger        :: Logger
    , runtimeProvider      :: Provider
    , runtimeState         :: MVar RuntimeState
    , runtimeStore         :: Store
    , runtimeRoutes        :: Routes
    , runtimeUniverse      :: Map Identifier (Compiler SomeItem)
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
            traverse_ (Logger.message logger . show) (M.keys todo)


--------------------------------------------------------------------------------
scheduleOutOfDate :: Runtime ()
scheduleOutOfDate = do
    logger   <- runtimeLogger   <$> ask
    provider <- runtimeProvider <$> ask
    universe <- runtimeUniverse <$> ask

    let identifiers = M.keys universe
        modified    = S.filter (resourceModified provider) (M.keysSet universe)

    state <- getRuntimeState
    let facts = runtimeFacts state
        todo  = runtimeTodo state
        done  = runtimeDone state

    let (ood, facts', msgs) = outOfDate identifiers modified facts
        todo'               = M.filterWithKey (\id' _ -> id' `S.member` ood) universe
        done'               = done `S.union` (M.keysSet universe `S.difference` ood)

    -- Print messages
    mapM_ (Logger.debug logger) msgs

    -- Update facts and todo items
    modifyRuntimeState $ \s -> s
        { runtimeDone  = done'
        , runtimeTodo  = todo `M.union` todo'
        , runtimeFacts = facts'
        }


--------------------------------------------------------------------------------
pickAndChase :: Runtime ()
pickAndChase = do
    todo <- runtimeTodo <$> getRuntimeState
    unless (null todo) $ do
        acted <- mconcat <$> forConcurrently (M.keys todo) chase
        when (acted == Idled) $ do
            -- This clause happens when chasing *every item* in `todo` resulted in 
            -- idling because tasks are all waiting on something: a dependency cycle  
            deps <- runtimeDependencies <$> getRuntimeState
            throwError $ "Hakyll.Core.Runtime.pickAndChase: Dependency cycle detected: " ++ 
                intercalate ", " [show k ++ " depends on " ++ show (S.toList v) | (k, v) <- M.toList deps]
        pickAndChase


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

    let compiler = (runtimeTodo state) M.! id'
        read' = CompilerRead
            { compilerConfig     = config
            , compilerUnderlying = id'
            , compilerProvider   = provider
            , compilerUniverse   = M.keysSet universe
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
                { runtimeSnapshots = S.insert (id', snapshot) (runtimeSnapshots s)
                , runtimeTodo      = M.insert id' c (runtimeTodo s)
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
                { runtimeDone         = S.insert id' (runtimeDone s)
                , runtimeTodo         = M.delete id' (runtimeTodo s)
                , runtimeFacts        = M.insert id' facts (runtimeFacts s)
                , runtimeDependencies = M.delete id' (runtimeDependencies s)
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
                        depId `S.member` done ||
                        (depId, depSnapshot) `S.member` snapshots
                    actualDep = [(depId, depSnapshot) | not depDone]

                return actualDep  

            modifyRuntimeState $ \s -> s
                { runtimeTodo         = M.insert id'
                    (if null deps then c else compilerResult result)
                    (runtimeTodo s)
                 -- We track dependencies only to inform users when an infinite loop is detected
                , runtimeDependencies = M.insertWith S.union id' (S.fromList deps) (runtimeDependencies s)
                }

            -- Progress has been made if at least one of the 
            -- requirements can move forwards at the next pass
            -- In some cases, dependencies have been processed in parallel in which case `deps` 
            -- can be empty, and we can progress to the next stage. See issue #907
            let progress | null deps    = Progressed
                         | deps == reqs = Idled
                         | otherwise    = Progressed

            return progress
