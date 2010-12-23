-- | Given a dependency graph, this module provides a function that will
-- generate an order in which the graph can be visited, so that all the
-- dependencies of a given node have been visited before the node itself is
-- visited.
--
module Hakyll.Core.DirectedGraph.DependencySolver
    ( solveDependencies
    ) where

import Prelude
import qualified Prelude as P
import Data.Set (Set)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S

import Hakyll.Core.DirectedGraph
import qualified Hakyll.Core.DirectedGraph as DG
import Hakyll.Core.DirectedGraph.Internal

-- | Solve a dependency graph. This function returns an order to run the
-- different nodes
--
solveDependencies :: Ord a
                  => DirectedGraph a  -- ^ Graph
                  -> [a]              -- ^ Resulting plan
solveDependencies = P.reverse . order [] [] S.empty

-- | Produce a reversed order using a stack
--
order :: Ord a
      => [a]              -- ^ Temporary result
      -> [Node a]         -- ^ Backtrace stack
      -> Set a            -- ^ Items in the stack
      -> DirectedGraph a  -- ^ Graph
      -> [a]              -- ^ Ordered result
order temp stack set graph@(DirectedGraph graph')
    -- Empty graph - return our current result
    | M.null graph' = temp
    | otherwise = case stack of

        -- Empty stack - pick a node, and add it to the stack
        [] ->
            let (tag, node) = M.findMin graph'
            in order temp (node : stack) (S.insert tag set) graph

        -- At least one item on the stack - continue using this item
        (node : stackTail) ->
            -- Check which dependencies are still in the graph
            let tag = nodeTag node
                deps = S.toList $ nodeNeighbours node
                unsatisfied = catMaybes $ map (flip M.lookup graph') deps
            in case unsatisfied of
                
                -- All dependencies for node are satisfied, we can return it and
                -- remove it from the graph
                [] -> order (tag : temp) stackTail (S.delete tag set)
                            (DG.filter (== tag) graph)

                -- There is at least one dependency left. We need to solve that
                -- one first...
                (dep : _) -> if (nodeTag dep) `S.member` set
                    -- The dependency is already in our stack - cycle detected!
                    then error "order: Cycle detected!"  -- TODO: Dump cycle
                    -- Continue with the dependency
                    else order temp (dep : node : stackTail)
                                    (S.insert (nodeTag dep) set)
                                    graph
