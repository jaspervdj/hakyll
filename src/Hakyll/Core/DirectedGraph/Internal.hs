-- | Internal structure of the DirectedGraph type. Not exported outside of the
-- library.
--
module Hakyll.Core.DirectedGraph.Internal
    ( Node (..)
    , DirectedGraph (..)
    ) where

import Prelude hiding (reverse, filter)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

-- | A node in the directed graph
--
data Node a = Node
    { nodeTag        :: a        -- ^ Tag identifying the node
    , nodeNeighbours :: (Set a)  -- ^ Edges starting at this node
    } deriving (Show)

-- | Append two nodes. Useful for joining graphs.
--
appendNodes :: Ord a => Node a -> Node a -> Node a
appendNodes (Node t1 n1) (Node t2 n2)
    | t1 /= t2 = error "appendNodes: Appending differently tagged nodes"
    | otherwise = Node t1 (n1 `S.union` n2)

-- | Type used to represent a directed graph
--
newtype DirectedGraph a = DirectedGraph {unDirectedGraph :: Map a (Node a)}
                        deriving (Show)

-- | Allow users to concatenate different graphs
--
instance Ord a => Monoid (DirectedGraph a) where
    mempty = DirectedGraph M.empty
    mappend (DirectedGraph m1) (DirectedGraph m2) = DirectedGraph $
        M.unionWith appendNodes m1 m2
