--------------------------------------------------------------------------------
module Hakyll.Core.Runtime
    ( run
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           ((<$>))
import           Control.Monad                 (filterM)
import           Control.Monad.Error           (ErrorT, runErrorT, throwError)
import           Control.Monad.Reader          (ask)
import           Control.Monad.RWS             (RWST, runRWST)
import           Control.Monad.State           (get, modify)
import           Control.Monad.Trans           (liftIO)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Monoid                   (mempty)
import           Data.Set                      (Set)
import qualified Data.Set                      as S
import           System.Exit                   (ExitCode (..), exitWith)
import           System.FilePath               ((</>))


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Compiler.Require
import           Hakyll.Core.Configuration
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
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
run :: Configuration -> Rules a -> IO RuleSet
run config rules = do
    -- Initialization
    logger <- Logger.new (verbosity config) putStrLn
    Logger.header logger "Initialising..."
    Logger.message logger "Creating store..."
    store <- Store.new (inMemoryCache config) $ storeDirectory config
    Logger.message logger "Creating provider..."
    provider <- newProvider store (ignoreFile config) $ providerDirectory config
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
            , runtimeUniverse      = compilers
            }
        state     = RuntimeState
            { runtimeDone  = S.empty
            , runtimeTodo  = M.empty
            , runtimeFacts = oldFacts
            }

    -- Run the program and fetch the resulting state
    result <- runErrorT $ runRWST build read' state
    case result of
        Left e          -> do
            Logger.error logger e
            Logger.flush logger
            exitWith $ ExitFailure 1

        Right (_, s, _) -> do
            Store.set store factsKey $ runtimeFacts s
            Logger.flush logger
            return ruleSet
  where
    factsKey = ["Hakyll.Core.Runtime.run", "facts"]


--------------------------------------------------------------------------------
data RuntimeRead = RuntimeRead
    { runtimeConfiguration :: Configuration
    , runtimeLogger        :: Logger
    , runtimeProvider      :: Provider
    , runtimeStore         :: Store
    , runtimeRoutes        :: Routes
    , runtimeUniverse      :: [(Identifier, Compiler SomeItem)]
    }


--------------------------------------------------------------------------------
data RuntimeState = RuntimeState
    { runtimeDone  :: Set Identifier
    , runtimeTodo  :: Map Identifier (Compiler SomeItem)
    , runtimeFacts :: DependencyFacts
    }


--------------------------------------------------------------------------------
type Runtime a = RWST RuntimeRead () RuntimeState (ErrorT String IO) a


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

    let identifiers = map fst universe
    modified <- fmap S.fromList $ flip filterM identifiers $
        liftIO . resourceModified provider
    let (ood, facts', msgs) = outOfDate identifiers modified facts
        todo'               = M.fromList
            [(id', c) | (id', c) <- universe, id' `S.member` ood]

    -- Print messages
    mapM_ (Logger.debug logger) msgs

    -- Update facts and todo items
    modify $ \s -> s
        { runtimeDone  = runtimeDone s `S.union`
            (S.fromList identifiers `S.difference` ood)
        , runtimeTodo  = todo `M.union` todo'
        , runtimeFacts = facts'
        }


--------------------------------------------------------------------------------
pickAndChase :: Runtime ()
pickAndChase = do
    todo <- runtimeTodo <$> get
    case M.minViewWithKey todo of
        Nothing            -> return ()
        Just ((id', _), _) -> do
            chase [] id'
            pickAndChase


--------------------------------------------------------------------------------
chase :: [Identifier] -> Identifier -> Runtime ()
chase trail id'
    | id' `elem` trail = return ()  -- Cycle detected!
    | otherwise        = do
        logger   <- runtimeLogger        <$> ask
        todo     <- runtimeTodo          <$> get
        provider <- runtimeProvider      <$> ask
        universe <- runtimeUniverse      <$> ask
        routes   <- runtimeRoutes        <$> ask
        store    <- runtimeStore         <$> ask
        config   <- runtimeConfiguration <$> ask

        let compiler = todo M.! id'
            read' = CompilerRead
                { compilerUnderlying = id'
                , compilerProvider   = provider
                , compilerUniverse   = map fst universe
                , compilerRoutes     = routes
                , compilerStore      = store
                , compilerLogger     = logger
                }

        result <- liftIO $ runCompiler compiler read'
        case result of
            -- Rethrow error
            CompilerError e -> throwError e

            -- Huge success
            CompilerDone (SomeItem item) cwrite -> do
                -- TODO: Sanity check on itemIdentifier?
                let facts     = compilerDependencies cwrite
                    cacheHits
                        | compilerCacheHits cwrite <= 0 = "updated"
                        | otherwise                     = "cached "

                -- Print some info
                Logger.message logger $ cacheHits ++ " " ++ show id'

                -- Write if necessary
                case runRoutes routes id' of
                    Nothing  -> return ()
                    Just url -> do
                        let path = destinationDirectory config </> url
                        liftIO $ makeDirectories path
                        liftIO $ write path item
                        Logger.debug logger $ "Routed to " ++ path

                -- Save! (For require)
                liftIO $ save store item

                -- Update state
                modify $ \s -> s
                    { runtimeDone  = S.insert id' (runtimeDone s)
                    , runtimeTodo  = M.delete id' (runtimeTodo s)
                    , runtimeFacts = M.insert id' facts (runtimeFacts s)
                    }

            -- Try something else first
            CompilerRequire dep c -> do
                -- Update the compiler so we don't execute it twice
                depDone <- (dep `S.member`) . runtimeDone <$> get
                modify $ \s -> s
                    { runtimeTodo = M.insert id'
                        (if depDone then c else compilerResult result)
                        (runtimeTodo s)
                    }

                -- If the required item is already compiled, continue, or, start
                -- chasing that
                if depDone then chase trail id' else chase (id' : trail) dep
