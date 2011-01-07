-- | Representation of a directed graph. In Hakyll, this is used for dependency
-- tracking.
--
module Hakyll.Core.DirectedGraph
    ( DirectedGraph
    , fromList
    , nodes
    , neighbours
    , reverse
    , reachableNodes
    ) where

import Prelude hiding (reverse)
import Data.Monoid (mconcat)
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Hakyll.Core.DirectedGraph.Internal

-- | Construction of directed graphs
--
fromList :: Ord a
         => [(a, Set a)]     -- ^ List of (node, reachable neighbours)
         -> DirectedGraph a  -- ^ Resulting directed graph
fromList = DirectedGraph . M.fromList . map (\(t, d) -> (t, Node t d))

-- | Get all nodes in the graph
--
nodes :: Ord a
      => DirectedGraph a  -- ^ Graph to get the nodes from
      -> Set a            -- ^ All nodes in the graph
nodes = M.keysSet . unDirectedGraph

-- | Get a set of reachable neighbours from a directed graph
--
neighbours :: Ord a
           => a                -- ^ Node to get the neighbours of
           -> DirectedGraph a  -- ^ Graph to search in
           -> Set a            -- ^ Set containing the neighbours
neighbours x = fromMaybe S.empty . fmap nodeNeighbours
             . M.lookup x . unDirectedGraph

-- | Reverse a directed graph (i.e. flip all edges)
--
reverse :: Ord a
        => DirectedGraph a
        -> DirectedGraph a
reverse = mconcat . map reverse' . M.toList . unDirectedGraph
  where
    reverse' (id', Node _ neighbours') = fromList $
        zip (S.toList neighbours') $ repeat $ S.singleton id'

-- | Find all reachable nodes from a given set of nodes in the directed graph
--
reachableNodes :: Ord a => Set a -> DirectedGraph a -> Set a
reachableNodes set graph = reachable (setNeighbours set) set
  where
    reachable next visited
        | S.null next = visited
        | otherwise = reachable (sanitize neighbours') (next `S.union` visited)
      where
        sanitize = S.filter (`S.notMember` visited)
        neighbours' = setNeighbours (sanitize next)

    setNeighbours = S.unions . map (flip neighbours graph) . S.toList
