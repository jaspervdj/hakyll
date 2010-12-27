-- | This is the module which binds it all together
--
module Hakyll.Core.Run where

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM_)
import qualified Data.Map as M

import Hakyll.Core.Route
import Hakyll.Core.Compiler
import Hakyll.Core.ResourceProvider
import Hakyll.Core.ResourceProvider.FileResourceProvider
import Hakyll.Core.Rules
import Hakyll.Core.Target
import Hakyll.Core.DirectedGraph
import Hakyll.Core.DirectedGraph.DependencySolver
import Hakyll.Core.Writable
import Hakyll.Core.Store

hakyll :: Writable a => Rules a -> IO ()
hakyll rules = do
    store <- makeStore "_store"
    provider <- fileResourceProvider
    hakyllWith rules provider store

hakyllWith :: Writable a => Rules a -> ResourceProvider -> Store -> IO ()
hakyllWith rules provider store = do
    let -- Get the rule set
        ruleSet = runRules rules provider

        -- Get all identifiers and compilers
        compilers = rulesCompilers ruleSet

        -- Get all targets
        targets = flip map compilers $ \(id', compiler) ->
            let (targ, deps) = runCompiler compiler id'
            in (id', targ, deps)

        -- Map mapping every identifier to it's target
        targetMap = M.fromList $ map (\(i, t, _) -> (i, t)) targets

        -- Create a dependency graph
        graph = fromList $ map (\(i, _, d) -> (i, d)) targets

        -- Solve the graph, creating a target order
        ordered = solveDependencies graph

        -- Join the order with the targets again
        orderedTargets = map (id &&& (targetMap M.!)) ordered

    -- Generate all the targets in order
    map' <- foldM addTarget M.empty orderedTargets

    let -- Fetch the routes
        route' = rulesRoute ruleSet

    forM_ (M.toList map') $ \(id', result) ->
        case runRoute route' id' of
            Nothing -> return ()
            Just r  -> do
                putStrLn $ "Routing " ++ show id' ++ " to " ++ r
                write r result

    putStrLn "DONE."
  where
    addTarget map' (id', targ) = do
        result <- runTarget targ id' (map' M.!) provider store
        putStrLn $ "Generated target: " ++ show id'
        return $ M.insert id' result map'
