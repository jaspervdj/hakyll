-- | Representation of a directed graph. In Hakyll, this is used for dependency
-- tracking.
--
module Hakyll.Core.DirectedGraph
    ( DirectedGraph
    , fromList
    , neighbours
    , reverse
    , filter
    , reachableNodes
    ) where

import Prelude hiding (reverse, filter)
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

-- | Filter a directed graph (i.e. remove nodes based on a predicate)
--
filter :: Ord a
       => (a -> Bool)      -- ^ Predicate
       -> DirectedGraph a  -- ^ Graph
       -> DirectedGraph a  -- ^ Resulting graph
filter predicate =
    DirectedGraph . M.filterWithKey (\k _ -> predicate k) . unDirectedGraph

-- | Find all reachable nodes from a given node in the directed graph
--
reachableNodes :: Ord a => a -> DirectedGraph a -> Set a
reachableNodes x graph = reachable (neighbours x graph) (S.singleton x)
  where
    reachable next visited
        | S.null next = visited
        | otherwise = reachable (sanitize neighbours') (next `S.union` visited)
      where
        sanitize = S.filter (`S.notMember` visited)
        neighbours' = S.unions $ map (flip neighbours graph)
                               $ S.toList $ sanitize next

{-
exampleGraph :: DirectedGraph Int
exampleGraph = fromList
    [ makeNode 8 [2, 4, 6]
    , makeNode 2 [4, 3]
    , makeNode 4 [3]
    , makeNode 6 [4]
    , makeNode 3 []
    ]
  where
    makeNode tag deps = (tag, S.fromList deps)

cyclic :: DirectedGraph Int
cyclic = fromList
    [ (1, S.fromList [2])
    , (2, S.fromList [1, 3])
    ]
-}
