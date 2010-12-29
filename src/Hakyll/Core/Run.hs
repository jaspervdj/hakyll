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
import Hakyll.Core.Util.File
import Hakyll.Core.Compiler
import Hakyll.Core.ResourceProvider
import Hakyll.Core.ResourceProvider.FileResourceProvider
import Hakyll.Core.Rules
import Hakyll.Core.Target
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

        -- Get all targets
        targets = flip map compilers $ \(id', compiler) ->
            let (targ, deps) = runCompiler compiler id' provider
            in (id', targ, deps)

        -- Map mapping every identifier to it's target
        targetMap = M.fromList $ map (\(i, t, _) -> (i, t)) targets

        -- Create a dependency graph
        graph = fromList $ map (\(i, _, d) -> (i, d)) targets

        -- Solve the graph, creating a target order
        ordered = solveDependencies graph

        -- Join the order with the targets again
        orderedTargets = map (id &&& (targetMap M.!)) ordered

        -- Fetch the routes
        route' = rulesRoute ruleSet

    putStrLn "Writing dependency graph to dependencies.dot..."
    writeDot "dependencies.dot" show graph

    -- Generate all the targets in order
    _ <- foldM (addTarget route') M.empty orderedTargets

    putStrLn "DONE."
  where
    addTarget route' map' (id', targ) = do
        compiled <- runTarget targ id' (dependencyLookup map') provider store
        putStrLn $ "Generated target: " ++ show id'

        case runRoute route' id' of
            Nothing -> return ()
            Just r  -> do
                putStrLn $ "Routing " ++ show id' ++ " to " ++ r
                let path = "_site" </> r
                makeDirectories path
                write path compiled

        return $ M.insert id' compiled map'

    dependencyLookup map' id' = case M.lookup id' map' of
        Nothing -> error $ "dependencyLookup: " ++ show id' ++ " not found"
        Just d  -> d
