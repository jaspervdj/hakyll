-- | This is the module which binds it all together
--
module Hakyll.Core.Run where

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM_)
import qualified Data.Map as M
import Data.Monoid (mempty)
import Data.Typeable (Typeable)
import Data.Binary (Binary)
import System.FilePath ((</>))

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

        -- Create and solve the graph, creating a compiler order
        graph = fromList dependencies
        ordered = solveDependencies graph

        -- Join the order with the compilers again
        orderedCompilers = map (id &&& (compilerMap M.!)) ordered

        -- Fetch the routes
        route' = rulesRoute ruleSet

    putStrLn "Writing dependency graph to dependencies.dot..."
    writeDot "dependencies.dot" show graph

    -- Generate all the targets in order
    _ <- foldM (addTarget route') M.empty orderedCompilers

    putStrLn "DONE."
  where
    addTarget route' map' (id', comp) = do
        let url = runRoute route' id'
        
        -- Check if the resource was modified
        modified <- if resourceExists provider id'
                        then resourceModified provider id' store
                        else return False

        -- Run the compiler
        compiled <- runCompilerJob comp id' provider (dependencyLookup map')
                                   url store modified
        putStrLn $ "Generated target: " ++ show id'

        case url of
            Nothing -> return ()
            Just r  -> do
                putStrLn $ "Routing " ++ show id' ++ " to " ++ r
                let path = "_site" </> r
                makeDirectories path
                write path compiled

        putStrLn ""
        return $ M.insert id' compiled map'

    dependencyLookup map' id' = case M.lookup id' map' of
        Nothing -> error $ "dependencyLookup: " ++ show id' ++ " not found"
        Just d  -> d
