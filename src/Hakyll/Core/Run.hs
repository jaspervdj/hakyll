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
import Control.Monad.State.Strict (StateT, runStateT, get, modify)
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Monoid (mempty, mappend)
import System.FilePath ((</>))
import Data.Set (Set)
import qualified Data.Set as S

import Hakyll.Core.Routes
import Hakyll.Core.Identifier
import Hakyll.Core.Util.File
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.ResourceProvider
import Hakyll.Core.ResourceProvider.FileResourceProvider
import Hakyll.Core.Rules.Internal
import Hakyll.Core.DirectedGraph
import Hakyll.Core.DirectedGraph.DependencySolver
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

    let ruleSet = runRules rules provider
        compilers = rulesCompilers ruleSet

        -- Extract the reader/state
        reader = unRuntime $ addNewCompilers [] compilers
        stateT = runReaderT reader $ env logger ruleSet provider store

    -- Run the program and fetch the resulting state
    ((), state') <- runStateT stateT state

    -- We want to save the final dependency graph for the next run
    storeSet store "Hakyll.Core.Run.run" "dependencies" $ hakyllGraph state'

    -- Flush and return
    flushLogger logger
    return ruleSet
  where
    env logger ruleSet provider store = RuntimeEnvironment
        { hakyllLogger           = logger
        , hakyllConfiguration    = configuration
        , hakyllRoutes           = rulesRoutes ruleSet
        , hakyllResourceProvider = provider
        , hakyllStore            = store
        }

    state = RuntimeState
        { hakyllModified = S.empty
        , hakyllGraph    = mempty
        }

data RuntimeEnvironment = RuntimeEnvironment
    { hakyllLogger           :: Logger
    , hakyllConfiguration    :: HakyllConfiguration
    , hakyllRoutes           :: Routes
    , hakyllResourceProvider :: ResourceProvider
    , hakyllStore            :: Store
    }

data RuntimeState = RuntimeState
    { hakyllModified :: Set Identifier
    , hakyllGraph    :: DirectedGraph Identifier
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
    if resourceExists provider id'
        then resourceModified provider (Resource id') store
        else return False

-- | Add a number of compilers and continue using these compilers
--
addNewCompilers :: [(Identifier, Compiler () CompileRule)]
                -- ^ Remaining compilers yet to be run
                -> [(Identifier, Compiler () CompileRule)]
                -- ^ Compilers to add
                -> Runtime ()
addNewCompilers oldCompilers newCompilers = Runtime $ do
    -- Get some information
    logger <- hakyllLogger <$> ask
    section logger "Adding new compilers"
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask

    let -- All compilers
        compilers = oldCompilers ++ newCompilers

        -- Get all dependencies for the compilers
        dependencies = flip map compilers $ \(id', compiler) ->
            let deps = runCompilerDependencies compiler id' provider
            in (id', deps)

        -- Create a compiler map (Id -> Compiler)
        compilerMap = M.fromList compilers

        -- Create the dependency graph
        currentGraph = fromList dependencies

    -- Find the old graph and append the new graph to it. This forms the
    -- complete graph
    completeGraph <- timed logger "Creating graph" $
        mappend currentGraph . hakyllGraph <$> get

    orderedCompilers <- timed logger "Solving dependencies" $ do
        -- Check which items are up-to-date. This only needs to happen for the new
        -- compilers
        oldModified <- hakyllModified <$> get 
        newModified <- liftIO $ modified provider store $ map fst newCompilers

        let modified' = oldModified `S.union` newModified
            
            -- Find obsolete items. Every item that is reachable from a modified
            -- item is considered obsolete. From these obsolete items, we are only
            -- interested in ones that are in the current subgraph.
            obsolete = S.filter (`member` currentGraph)
                     $ reachableNodes modified' $ reverse completeGraph

            -- Solve the graph and retain only the obsolete items
            ordered = filter (`S.member` obsolete) $ solveDependencies currentGraph

        -- Update the state
        modify $ updateState modified' completeGraph

        -- Join the order with the compilers again
        return $ map (id &&& (compilerMap M.!)) ordered

    -- Now run the ordered list of compilers
    unRuntime $ runCompilers orderedCompilers
  where
    -- Add the modified information for the new compilers
    updateState modified' graph state = state
        { hakyllModified = modified'
        , hakyllGraph    = graph
        }

runCompilers :: [(Identifier, Compiler () CompileRule)]
             -- ^ Ordered list of compilers
             -> Runtime ()
             -- ^ No result
runCompilers [] = return ()
runCompilers ((id', compiler) : compilers) = Runtime $ do
    -- Obtain information
    logger <- hakyllLogger <$> ask
    routes <- hakyllRoutes <$> ask
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask
    modified' <- hakyllModified <$> get

    section logger $ "Compiling " ++ show id'

    let -- Check if the resource was modified
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
            unRuntime $ runCompilers compilers 

        -- Metacompiler, slightly more complicated
        Right (MetaCompileRule newCompilers) ->
            -- Actually I was just kidding, it's not hard at all
            unRuntime $ addNewCompilers compilers newCompilers

        -- Some error happened, log and continue
        Left err -> do
            thrown logger err 
            unRuntime $ runCompilers compilers
