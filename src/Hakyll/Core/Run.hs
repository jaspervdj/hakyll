-- | This is the module which binds it all together
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Hakyll.Core.Run
    ( run
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad (filterM, forM_)
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Strict (StateT, runStateT, get, put)
import Control.Monad.Trans (liftIO)
import Data.Map (Map)
import Data.Monoid (mempty, mappend)
import Prelude hiding (reverse)
import System.FilePath ((</>))
import qualified Data.Map as M
import qualified Data.Set as S

import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Configuration
import Hakyll.Core.DependencyAnalyzer
import Hakyll.Core.DirectedGraph
import Hakyll.Core.Identifier
import Hakyll.Core.Logger
import Hakyll.Core.Resource
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Resource.Provider.File
import Hakyll.Core.Routes
import Hakyll.Core.Rules.Internal
import Hakyll.Core.Store
import Hakyll.Core.Util.File
import Hakyll.Core.Writable

-- | Run all rules needed, return the rule set used
--
run :: HakyllConfiguration -> RulesM a -> IO RuleSet
run configuration rules = do
    logger <- makeLogger putStrLn

    section logger "Initialising"
    store <- timed logger "Creating store" $
        makeStore (inMemoryCache configuration) $ storeDirectory configuration
    provider <- timed logger "Creating provider" $
        fileResourceProvider configuration

    -- Fetch the old graph from the store. If we don't find it, we consider this
    -- to be the first run
    graph <- storeGet store "Hakyll.Core.Run.run" "dependencies"
    let (firstRun, oldGraph) = case graph of Found g -> (False, g)
                                             _       -> (True, mempty)

    let ruleSet = runRules rules provider
        compilers = rulesCompilers ruleSet

        -- Extract the reader/state
        reader = unRuntime $ addNewCompilers compilers
        stateT = runReaderT reader $ RuntimeEnvironment
                    { hakyllLogger           = logger
                    , hakyllConfiguration    = configuration
                    , hakyllRoutes           = rulesRoutes ruleSet
                    , hakyllResourceProvider = provider
                    , hakyllStore            = store
                    , hakyllFirstRun         = firstRun
                    }

    -- Run the program and fetch the resulting state
    result <- runErrorT $ runStateT stateT $ RuntimeState
        { hakyllAnalyzer  = makeDependencyAnalyzer mempty (const False) oldGraph
        , hakyllCompilers = M.empty
        }

    case result of
        Left e             ->
            thrown logger e
        Right ((), state') ->
            -- We want to save the final dependency graph for the next run
            storeSet store "Hakyll.Core.Run.run" "dependencies" $
                analyzerGraph $ hakyllAnalyzer state'

    -- Flush and return
    flushLogger logger
    return ruleSet

data RuntimeEnvironment = RuntimeEnvironment
    { hakyllLogger           :: Logger
    , hakyllConfiguration    :: HakyllConfiguration
    , hakyllRoutes           :: Routes
    , hakyllResourceProvider :: ResourceProvider
    , hakyllStore            :: Store
    , hakyllFirstRun         :: Bool
    }

data RuntimeState = RuntimeState
    { hakyllAnalyzer  :: DependencyAnalyzer (Identifier ())
    , hakyllCompilers :: Map (Identifier ()) (Compiler () CompileRule)
    }

newtype Runtime a = Runtime
    { unRuntime :: ReaderT RuntimeEnvironment
        (StateT RuntimeState (ErrorT String IO)) a
    } deriving (Functor, Applicative, Monad)

-- | Add a number of compilers and continue using these compilers
--
addNewCompilers :: [(Identifier (), Compiler () CompileRule)]
                -- ^ Compilers to add
                -> Runtime ()
addNewCompilers newCompilers = Runtime $ do
    -- Get some information
    logger <- hakyllLogger <$> ask
    section logger "Adding new compilers"
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask
    firstRun <- hakyllFirstRun <$> ask

    -- Old state information
    oldCompilers <- hakyllCompilers <$> get
    oldAnalyzer <- hakyllAnalyzer <$> get

    let -- All known compilers
        universe = M.keys oldCompilers ++ map fst newCompilers

        -- Create a new partial dependency graph
        dependencies = flip map newCompilers $ \(id', compiler) ->
            let deps = runCompilerDependencies compiler id' universe
            in (id', deps)

        -- Create the dependency graph
        newGraph = fromList dependencies

    -- Check which items have been modified
    modified <- fmap S.fromList $ flip filterM (map fst newCompilers) $
        liftIO . resourceModified provider store . fromIdentifier
    let checkModified = if firstRun then const True else (`S.member` modified)

    -- Create a new analyzer and append it to the currect one
    let newAnalyzer = makeDependencyAnalyzer newGraph checkModified $
            analyzerPreviousGraph oldAnalyzer
        analyzer = mappend oldAnalyzer newAnalyzer

    -- Update the state
    put $ RuntimeState
        { hakyllAnalyzer  = analyzer
        , hakyllCompilers = M.union oldCompilers (M.fromList newCompilers)
        }

    -- Continue
    unRuntime stepAnalyzer

stepAnalyzer :: Runtime ()
stepAnalyzer = Runtime $ do
    -- Step the analyzer
    state <- get
    let (signal, analyzer') = step $ hakyllAnalyzer state
    put $ state { hakyllAnalyzer = analyzer' }

    case signal of Done      -> return ()
                   Cycle c   -> unRuntime $ dumpCycle c
                   Build id' -> unRuntime $ build id'

-- | Dump cyclic error and quit
--
dumpCycle :: [Identifier ()] -> Runtime ()
dumpCycle cycle' = Runtime $ do
    logger <- hakyllLogger <$> ask
    section logger "Dependency cycle detected! Conflict:"
    forM_ (zip cycle' $ drop 1 cycle') $ \(x, y) ->
        report logger $ show x ++ " -> " ++ show y

build :: Identifier () -> Runtime ()
build id' = Runtime $ do
    logger <- hakyllLogger <$> ask
    routes <- hakyllRoutes <$> ask
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask
    compilers <- hakyllCompilers <$> get

    section logger $ "Compiling " ++ show id'

    -- Fetch the right compiler from the map
    let compiler = compilers M.! id'

    -- Check if the resource was modified
    isModified <- liftIO $ resourceModified provider store $ fromIdentifier id'

    -- Run the compiler
    result <- timed logger "Total compile time" $ liftIO $
        runCompiler compiler id' provider (M.keys compilers) routes
                    store isModified logger

    case result of
        -- Compile rule for one item, easy stuff
        Right (CompileRule compiled) -> do
            case runRoutes routes id' of
                Nothing  -> return ()
                Just url -> timed logger ("Routing to " ++ url) $ do
                    destination <-
                        destinationDirectory . hakyllConfiguration <$> ask
                    let path = destination </> url
                    liftIO $ makeDirectories path
                    liftIO $ write path compiled

            -- Continue for the remaining compilers
            unRuntime stepAnalyzer

        -- Metacompiler, slightly more complicated
        Right (MetaCompileRule newCompilers) ->
            -- Actually I was just kidding, it's not hard at all
            unRuntime $ addNewCompilers newCompilers

        -- Some error happened, rethrow in Runtime monad
        Left err -> throwError err
