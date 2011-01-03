-- | This is the module which binds it all together
--
module Hakyll.Core.Run where

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM_, forM, filterM)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (mempty)
import Data.Typeable (Typeable)
import Data.Binary (Binary)
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import Data.Set (Set)
import qualified Data.Set as S

import Hakyll.Core.Route
import Hakyll.Core.Identifier
import Hakyll.Core.Util.File
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.ResourceProvider
import Hakyll.Core.ResourceProvider.FileResourceProvider
import Hakyll.Core.Rules
import Hakyll.Core.DirectedGraph
import Hakyll.Core.DirectedGraph.Dot
import Hakyll.Core.DirectedGraph.DependencySolver
import Hakyll.Core.DirectedGraph.ObsoleteFilter
import Hakyll.Core.Writable
import Hakyll.Core.Store
import Hakyll.Core.CompiledItem

hakyll :: Rules -> IO ()
hakyll rules = do
    store <- makeStore "_store"
    provider <- fileResourceProvider
    hakyllWith rules provider store

hakyllWith :: Rules -> ResourceProvider -> Store -> IO ()
hakyllWith rules provider store = do
    let -- Get the rule set
        ruleSet = runRules rules provider

        -- Get all identifiers and compilers
        compilers = rulesCompilers ruleSet

        -- Get all dependencies
        dependencies = flip map compilers $ \(id', compiler) ->
            let deps = runCompilerDependencies compiler provider
            in (id', deps)

        -- Create a compiler map
        compilerMap = M.fromList compilers

        -- Create the graph
        graph = fromList dependencies

    putStrLn "Writing dependency graph to dependencies.dot..."
    writeDot "dependencies.dot" show graph

    -- Check which items are up-to-date
    modified' <- modified provider store $ map fst compilers

    let -- Try to reduce the graph
        reducedGraph = filterObsolete modified' graph

    putStrLn "Writing reduced graph to reduced.dot..."
    writeDot "reduced.dot" show reducedGraph

    let -- Solve the graph
        ordered = solveDependencies reducedGraph

        -- Join the order with the compilers again
        orderedCompilers = map (id &&& (compilerMap M.!)) ordered

        -- Fetch the routes
        route' = rulesRoute ruleSet

    putStrLn $ show reducedGraph
    putStrLn $ show ordered

    -- Generate all the targets in order
    _ <- foldM (addTarget route' modified') M.empty orderedCompilers

    putStrLn "DONE."
  where
    addTarget route' modified' map' (id', comp) = do
        let url = runRoute route' id'
        
        -- Check if the resource was modified
        let isModified = id' `S.member` modified'

        -- Run the compiler
        compiled <- runCompilerJob comp id' provider (dependencyLookup map')
                                   url store isModified
        putStrLn $ "Generated target: " ++ show id'

        case url of
            Nothing -> return ()
            Just r  -> do
                putStrLn $ "Routing " ++ show id' ++ " to " ++ r
                let path = "_site" </> r
                makeDirectories path
                write path compiled

        -- Store it in the cache
        storeResult store id' compiled

        putStrLn ""
        return $ M.insert id' compiled map'

    dependencyLookup map' id' = M.lookup id' map'

modified :: ResourceProvider     -- ^ Resource provider
         -> Store                -- ^ Store
         -> [Identifier]         -- ^ Identifiers to check
         -> IO (Set Identifier)  -- ^ Modified resources
modified provider store ids = fmap S.fromList $ flip filterM ids $ \id' ->
    if resourceExists provider id' then resourceModified provider id' store
                                   else return False
