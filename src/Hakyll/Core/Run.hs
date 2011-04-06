-- | This is the module which binds it all together
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Hakyll.Core.Run
    ( run
    ) where

import Prelude hiding (reverse)
import Control.Monad (filterM)
import Control.Monad.Trans (liftIO)
import Control.Applicative (Applicative, (<$>))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Strict (StateT, runStateT, get, put)
import Control.Arrow ((&&&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (mempty, mappend)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import Data.Set (Set)
import qualified Data.Set as S

import Hakyll.Core.Routes
import Hakyll.Core.Identifier
import Hakyll.Core.Util.File
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Resource
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Resource.Provider.File
import Hakyll.Core.Rules.Internal
import Hakyll.Core.DirectedGraph
import Hakyll.Core.DirectedGraph.Dot
import Hakyll.Core.DirectedGraph.DependencySolver
import Hakyll.Core.DependencyAnalyzer
import Hakyll.Core.Writable
import Hakyll.Core.Store
import Hakyll.Core.Configuration
import Hakyll.Core.Logger

-- | Run all rules needed, return the rule set used
--
run :: HakyllConfiguration -> Rules -> IO RuleSet
run configuration rules = do
    logger <- makeLogger

    section logger "Initialising"
    store <- timed logger "Creating store" $
        makeStore $ storeDirectory configuration
    provider <- timed logger "Creating provider" $
        fileResourceProvider configuration

    -- Fetch the old graph from the store
    oldGraph <- fromMaybe mempty <$>
        storeGet store "Hakyll.Core.Run.run" "dependencies"

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
                    , hakyllOldGraph         = oldGraph
                    }

    -- Run the program and fetch the resulting state
    ((), state') <- runStateT stateT $ RuntimeState
        { hakyllAnalyzer  = makeDependencyAnalyzer mempty (const False) oldGraph
        , hakyllCompilers = M.empty
        , hakyllModified  = S.empty
        }

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
    , hakyllOldGraph         :: DirectedGraph Identifier
    }

data RuntimeState = RuntimeState
    { hakyllAnalyzer  :: DependencyAnalyzer Identifier
    , hakyllCompilers :: Map Identifier (Compiler () CompileRule)
    , hakyllModified  :: Set Identifier
    }

newtype Runtime a = Runtime
    { unRuntime :: ReaderT RuntimeEnvironment (StateT RuntimeState IO) a
    } deriving (Functor, Applicative, Monad)

-- | Return a set of modified identifiers
--
modified :: ResourceProvider     -- ^ Resource provider
         -> Store                -- ^ Store
         -> [Identifier]         -- ^ Identifiers to check
         -> IO (Set Identifier)  -- ^ Modified resources
modified provider store ids = fmap S.fromList $ flip filterM ids $ \id' ->
    resourceModified provider (Resource id') store

-- | Add a number of compilers and continue using these compilers
--
addNewCompilers :: [(Identifier, Compiler () CompileRule)]
                -- ^ Compilers to add
                -> Runtime ()
addNewCompilers newCompilers = Runtime $ do
    -- Get some information
    logger <- hakyllLogger <$> ask
    section logger "Adding new compilers"
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask

    -- Old state information
    oldCompilers <- hakyllCompilers <$> get
    oldAnalyzer <- hakyllAnalyzer <$> get
    oldModified <- hakyllModified <$> get

    let -- Create a new partial dependency graph
        dependencies = flip map newCompilers $ \(id', compiler) ->
            let deps = runCompilerDependencies compiler id' provider
            in (id', deps)

        -- Create the dependency graph
        newGraph = fromList dependencies

    -- Check which items have been modified
    newModified <- liftIO $ modified provider store $ map fst newCompilers

    -- Create a new analyzer and append it to the currect one
    let newAnalyzer =
            makeDependencyAnalyzer newGraph (`S.member` newModified) mempty
        analyzer = mappend oldAnalyzer newAnalyzer

    -- Update the state
    put $ RuntimeState
        { hakyllAnalyzer  = analyzer
        , hakyllCompilers = M.union oldCompilers (M.fromList newCompilers)
        , hakyllModified  = S.union oldModified newModified
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
                   Cycle c   -> return ()
                   Build id' -> unRuntime $ build id'

build :: Identifier -> Runtime ()
build id' = Runtime $ do
    logger <- hakyllLogger <$> ask
    routes <- hakyllRoutes <$> ask
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask
    modified' <- hakyllModified <$> get
    compilers <- hakyllCompilers <$> get

    section logger $ "Compiling " ++ show id'

    let -- Fetch the right compiler from the map
        compiler = compilers M.! id'

        -- Check if the resource was modified
        isModified = id' `S.member` modified'

    -- Run the compiler
    result <- timed logger "Total compile time" $ liftIO $
        runCompiler compiler id' provider routes store isModified logger

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

        -- Some error happened, log and continue
        Left err -> do
            thrown logger err 
            unRuntime stepAnalyzer
