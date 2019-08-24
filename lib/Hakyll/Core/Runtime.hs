--------------------------------------------------------------------------------
module Hakyll.Core.Runtime
    ( run
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent            (yield)
import           Control.Concurrent.Async      (mapConcurrently)
import           Control.Concurrent.STM
import           Control.Exception.Base        (Exception)
import           Control.Monad                 (unless)
import           Control.Monad.Except          (ExceptT, runExceptT, throwError)
import           Control.Monad.Reader          (ask)
import           Control.Monad.RWS             (RWST, runRWST)
import qualified Control.Monad.State as State
import           Control.Monad.Trans           (liftIO)
import           Data.List                     (intercalate)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Set                      (Set)
import qualified Data.Set                      as S
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


--------------------------------------------------------------------------------
run :: Configuration -> Logger -> Rules a -> IO (ExitCode, RuleSet)
run config logger rules = do
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
    let compilers = rulesCompilers ruleSet
        read'     = RuntimeRead
            { runtimeConfiguration = config
            , runtimeLogger        = logger
            , runtimeProvider      = provider
            , runtimeStore         = store
            , runtimeRoutes        = rulesRoutes ruleSet
            , runtimeUniverse      = M.fromList compilers
            }
    state <- newTVarIO $ RuntimeState'
            { runtimeDone      = S.empty
            , runtimeSnapshots = S.empty
            , runtimeTodo      = M.empty
            , runtimeDeps      = M.empty
            , runtimeFacts     = oldFacts
            }

    -- Run the program and fetch the resulting state
    result <- runExceptT $ runRWST build read' state
    case result of
        Left e          -> do
            Logger.error logger e
            Logger.flush logger
            return (ExitFailure 1, ruleSet)

        Right (_, sVar, _) -> do
            s <- readTVarIO sVar
            Store.set store factsKey $ runtimeFacts s

            Logger.debug logger "Removing tmp directory..."
            removeDirectory $ tmpDirectory config

            Logger.flush logger
            return (ExitSuccess, ruleSet)
  where
    factsKey = ["Hakyll.Core.Runtime.run", "facts"]


--------------------------------------------------------------------------------
data RuntimeRead = RuntimeRead
    { runtimeConfiguration :: Configuration
    , runtimeLogger        :: Logger
    , runtimeProvider      :: Provider
    , runtimeStore         :: Store
    , runtimeRoutes        :: Routes
    , runtimeUniverse      :: Map Identifier (Compiler SomeItem)
    }


--------------------------------------------------------------------------------
data RuntimeState' = RuntimeState'
    { runtimeDone      :: Set Identifier
    , runtimeSnapshots :: Set (Identifier, Snapshot)
    , runtimeTodo      :: Map Identifier (Compiler SomeItem)
    , runtimeDeps      :: Map Identifier (S.Set Identifier)
    , runtimeFacts     :: DependencyFacts
    }

type RuntimeState = TVar RuntimeState'
--------------------------------------------------------------------------------
type Runtime a = RWST RuntimeRead () RuntimeState (ExceptT String IO) a


--------------------------------------------------------------------------------
build :: Runtime ()
build = do
    logger <- runtimeLogger <$> ask
    Logger.header logger "Checking for out-of-date items"
    scheduleOutOfDate
    Logger.header logger "Compiling"
    pickAndChase
    Logger.header logger "Success"


--------------------------------------------------------------------------------
scheduleOutOfDate :: Runtime ()
scheduleOutOfDate = do
    logger   <- runtimeLogger   <$> ask
    provider <- runtimeProvider <$> ask
    universe <- runtimeUniverse <$> ask
    facts    <- runtimeFacts    <$> get
    todo     <- runtimeTodo     <$> get

    let identifiers = M.keys universe
        modified    = S.fromList $ flip filter identifiers $
            resourceModified provider

    let (ood, facts', msgs) = outOfDate identifiers modified facts
        todo'               = M.filterWithKey
            (\id' _ -> id' `S.member` ood) universe

    -- Print messages
    mapM_ (Logger.debug logger) msgs

    let todos = todo `M.union` todo'
    let deps  = M.fromList . fmap (\k -> (k, mempty)) $ M.keys todos

    -- Update facts and todo items
    modify $ \s -> s
        { runtimeDone  = runtimeDone s `S.union`
            (S.fromList identifiers `S.difference` ood)
        , runtimeTodo  = todos
        , runtimeDeps  = deps
        , runtimeFacts = facts'
        }


--------------------------------------------------------------------------------
pickAndChase :: Runtime ()
pickAndChase = do
    r     <- ask
    s     <- State.get
    todo  <- liftIO $ runtimeTodo <$> readTVarIO s
    let runChase id' = runExceptT (runRWST (chase id') r s)
    _ <- liftIO . mapConcurrently runChase $ (M.keys todo)
    pure ()

--------------------------------------------------------------------------------
chase :: Identifier -> Runtime ()
chase id' = do
  logger   <- runtimeLogger        <$> ask
  todo     <- runtimeTodo          <$> get
  provider <- runtimeProvider      <$> ask
  universe <- runtimeUniverse      <$> ask
  routes   <- runtimeRoutes        <$> ask
  store    <- runtimeStore         <$> ask
  config   <- runtimeConfiguration <$> ask
  Logger.debug logger $ "Processing " ++ show id'

  let compiler = todo M.! id'
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
  handleResult id' result

--------------------------------------------------------------------------------
handleResult :: Identifier -> (CompilerResult SomeItem) -> Runtime ()
handleResult id' result = do
  logger   <- runtimeLogger        <$> ask
  provider <- runtimeProvider      <$> ask
  routes   <- runtimeRoutes        <$> ask
  store    <- runtimeStore         <$> ask
  config   <- runtimeConfiguration <$> ask
  case result of
      -- Rethrow error
      CompilerError [] -> throwError
          "Compiler failed but no info given, try running with -v?"
      CompilerError es -> throwError $ intercalate "; " es

      -- Signal that a snapshot was saved ->
      CompilerSnapshot snapshot c -> do
          -- Update info. The next 'chase' will pick us again at some
          -- point so we can continue then.
          Logger.debug logger $ "Compiled snapshot of: " ++ show id'
          modify $ \s -> s
              { runtimeSnapshots =
                  S.insert (id', snapshot) (runtimeSnapshots s)
              , runtimeTodo      = M.insert id' c (runtimeTodo s)
              }
          chase id'

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

          Logger.debug logger $ "Fully compiled: " ++ show id'
          -- Update state
          modify $ \s -> s
              { runtimeDone  = S.insert id' (runtimeDone s)
              , runtimeTodo  = M.delete id' (runtimeTodo s)
              , runtimeFacts = M.insert id' facts (runtimeFacts s)
              }

      -- Try something else first
      CompilerRequire dep c -> do
          -- Update the compiler so we don't execute it twice
          let (depId, depSnapshot) = dep
          Logger.debug logger $
            "Compiler requirement found for: " ++ show id' ++
            ", requirement: " ++ show depId
          var <- State.get
          (action, depDone) <- liftIO . atomically $ do
            done      <- runtimeDone      <$> readTVar var
            snapshots <- runtimeSnapshots <$> readTVar var

            -- Done if we either completed the entire item (runtimeDone) or
            -- if we previously saved the snapshot (runtimeSnapshots).
            let depDone =
                    depId `S.member` done ||
                     (depId, depSnapshot) `S.member` snapshots
            deps <- runtimeDeps <$> readTVar var
            otherDeps <- case M.lookup depId deps of
                           Nothing -> missingDepError depId
                           Just deps' -> pure deps'

            let depState = case (depDone, id' `S.member` otherDeps) of
                             (_, True)  -> DepCycle
                             (True, _)  -> DepDone
                             (False, _) -> DepRequired

            modifyTVar' var $ \s -> s {
                  runtimeTodo = M.insert id'
                    (if depDone then c else compilerResult result)
                    (runtimeTodo s)
                , runtimeDeps =
                    M.adjust (S.insert depId) id' (runtimeDeps s)
                }

            let runtimeAction = case depState of
                   DepCycle -> dependencyCycleError id' depId
                   DepDone  -> chase id'
                   DepRequired -> do
                     liftIO $ yield
                     chase id'
            pure (runtimeAction, depDone)

          -- If the required item is already compiled, continue, or, start
          -- chasing that
          Logger.debug logger $ "Require " ++ show depId ++
              " (snapshot " ++ depSnapshot ++ "): " ++
              (if depDone then "OK" else "chasing")
          action

--------------------------------------------------------------------------------
-- WARNING: It is very easy to accidentally use an out-of-date
-- version of RuntimeState' when updating it. Be very careful
-- that your modify function only uses the provided reference
modify :: (RuntimeState' -> RuntimeState') -> Runtime ()
modify f = do
  var <- State.get
  liftIO . atomically $ do
    modifyTVar' var f

--------------------------------------------------------------------------------
get :: Runtime RuntimeState'
get = State.get >>= \var -> liftIO (readTVarIO var)

--------------------------------------------------------------------------------
data DependencyState = DepDone | DepRequired | DepCycle

--------------------------------------------------------------------------------
dependencyCycleError :: Identifier -> Identifier -> Runtime ()
dependencyCycleError l r = throwError msg where
  msg = "Dependency cycle detected: " ++ show l ++
        " depends on " ++ show r

--------------------------------------------------------------------------------
missingDepError :: Identifier -> STM a
missingDepError id' = throwSTM (RuntimePreconditionException msg) where
  msg = "Hakyll.Core.Runtime precondition exception: dependency " ++
        "set missing for " ++ show id' ++ ". You may need to clean and " ++
        "rebuild your site."

--------------------------------------------------------------------------------
data RuntimePreconditionException =
  RuntimePreconditionException String deriving Show

instance Exception RuntimePreconditionException
