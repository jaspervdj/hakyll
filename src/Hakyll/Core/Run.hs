--------------------------------------------------------------------------------
-- | This is the module which binds it all together
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Core.Run
    ( run
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            (Applicative, (<$>))
import           Control.DeepSeq                (deepseq)
import           Control.Monad                  (filterM, forM_)
import           Control.Monad.Error            (ErrorT, runErrorT, throwError)
import           Control.Monad.Reader           (ReaderT, ask, runReaderT)
import           Control.Monad.Trans            (liftIO)
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Monoid                    (mempty)
import qualified Data.Set                       as S
import           Prelude                        hiding (reverse)
import           System.FilePath                ((</>))


--------------------------------------------------------------------------------
import           Hakyll.Core.CompiledItem
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Configuration
import           Hakyll.Core.DependencyAnalyzer
import qualified Hakyll.Core.DirectedGraph      as DG
import           Hakyll.Core.Identifier
import           Hakyll.Core.Logger
import           Hakyll.Core.ResourceProvider
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Store              (Store)
import qualified Hakyll.Core.Store              as Store
import           Hakyll.Core.Util.File
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | Run all rules needed, return the rule set used
run :: HakyllConfiguration -> RulesM a -> IO RuleSet
run configuration rules = do
    logger <- makeLogger putStrLn

    section logger "Initialising"
    store <- timed logger "Creating store" $
        Store.new (inMemoryCache configuration) $ storeDirectory configuration
    provider <- timed logger "Creating provider" $ newResourceProvider
        store (ignoreFile configuration) "."

    ruleSet <- timed logger "Running rules" $ runRules rules provider
    let compilers = rulesCompilers ruleSet

        -- Extract the reader/state
        reader = unRuntime analyzeAndBuild
        errorT = runReaderT reader $ RuntimeEnvironment
                    { runtimeLogger        = logger
                    , runtimeConfiguration = configuration
                    , runtimeRoutes        = rulesRoutes ruleSet
                    , runtimeProvider      = provider
                    , runtimeStore         = store
                    , runtimeCompilers     = M.fromList compilers
                    }

    -- Run the program and fetch the resulting state
    result <- runErrorT errorT
    case result of
        Left e -> thrown logger e
        _      -> return ()

    -- Flush and return
    flushLogger logger
    return ruleSet


--------------------------------------------------------------------------------
data RuntimeEnvironment = RuntimeEnvironment
    { runtimeLogger        :: Logger
    , runtimeConfiguration :: HakyllConfiguration
    , runtimeRoutes        :: Routes
    , runtimeProvider      :: ResourceProvider
    , runtimeStore         :: Store
    , runtimeCompilers     :: Map (Identifier ()) (Compiler () CompiledItem)
    }


--------------------------------------------------------------------------------
newtype Runtime a = Runtime
    { unRuntime :: ReaderT RuntimeEnvironment (ErrorT String IO) a
    } deriving (Functor, Applicative, Monad)


--------------------------------------------------------------------------------
analyzeAndBuild :: Runtime ()
analyzeAndBuild = Runtime $ do
    -- Get some stuff
    logger    <- runtimeLogger <$> ask
    provider  <- runtimeProvider <$> ask
    store     <- runtimeStore <$> ask
    compilers <- runtimeCompilers <$> ask

    -- Checking which items have been modified
    let universe = M.keys compilers
    modified <- timed logger "Checking for modified items" $
        fmap S.fromList $ flip filterM universe $
            liftIO . resourceModified provider

    -- Fetch the old graph from the store. If we don't find it, we consider this
    -- to be the first run
    mOldGraph <- liftIO $ Store.get store graphKey
    let (firstRun, oldGraph) = case mOldGraph of Store.Found g -> (False, g)
                                                 _             -> (True, mempty)

        -- Create a new dependency graph
        graph    = DG.fromList $
            flip map (M.toList compilers) $ \(id', compiler) ->
                let deps = runCompilerDependencies compiler id' universe
                in (id', S.toList deps)

        ood | firstRun  = const True
            | otherwise = (`S.member` modified)

        -- Check for cycles and analyze the graph
        analysis = analyze oldGraph graph ood

    -- Make sure this stuff is evaluated
    () <- timed logger "Analyzing dependency graph" $
        oldGraph `deepseq` analysis `deepseq` return ()

    -- We want to save the new dependency graph for the next run
    liftIO $ Store.set store graphKey graph

    case analysis of
        Cycle c -> unRuntime $ dumpCycle c
        Order o -> mapM_ (unRuntime . build) o
  where
    graphKey = ["Hakyll.Core.Run.run", "dependencies"]


--------------------------------------------------------------------------------
-- | Dump cyclic error and quit
dumpCycle :: [Identifier ()] -> Runtime ()
dumpCycle cycle' = Runtime $ do
    logger <- runtimeLogger <$> ask
    section logger "Dependency cycle detected! Conflict:"
    forM_ (zip cycle' $ drop 1 cycle') $ \(x, y) ->
        report logger $ show x ++ " -> " ++ show y


--------------------------------------------------------------------------------
build :: Identifier () -> Runtime ()
build id' = Runtime $ do
    logger    <- runtimeLogger <$> ask
    routes    <- runtimeRoutes <$> ask
    provider  <- runtimeProvider <$> ask
    store     <- runtimeStore <$> ask
    compilers <- runtimeCompilers <$> ask

    section logger $ "Compiling " ++ show id'

    -- Fetch the right compiler from the map
    let compiler = compilers M.! id'

    -- Check if the resource was modified
    isModified <- liftIO $ resourceModified provider id'

    -- Run the compiler
    result <- timed logger "Total compile time" $ liftIO $
        runCompiler compiler id' provider (M.keys compilers) routes
                    store isModified logger

    case result of
        -- Success
        Right compiled -> do
            case runRoutes routes id' of
                Nothing  -> return ()
                Just url -> timed logger ("Routing to " ++ url) $ do
                    destination <-
                        destinationDirectory . runtimeConfiguration <$> ask
                    let path = destination </> url
                    liftIO $ makeDirectories path
                    liftIO $ write path compiled

        -- Some error happened, rethrow in Runtime monad
        Left err -> throwError err
