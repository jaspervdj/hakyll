-- | Internal structure of the DirectedGraph type. Not exported outside of the
-- library.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.DirectedGraph.Internal
    ( Node (..)
    , DirectedGraph (..)
    ) where

import Prelude hiding (reverse, filter)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid, mempty, mappend)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Binary (Binary, put, get)

-- | A node in the directed graph
--
data Node a = Node
    { nodeTag        :: a      -- ^ Tag identifying the node
    , nodeNeighbours :: Set a  -- ^ Edges starting at this node
    } deriving (Show)

instance (Binary a, Ord a) => Binary (Node a) where
    put (Node t n) = put t >> put n
    get = Node <$> get <*> get

-- | Append two nodes. Useful for joining graphs.
--
appendNodes :: Ord a => Node a -> Node a -> Node a
appendNodes (Node t1 n1) (Node t2 n2)
    | t1 /= t2 = error'
    | otherwise = Node t1 (n1 `S.union` n2)
  where
    error' = error $  "Hakyll.Core.DirectedGraph.Internal.appendNodes: "
                   ++ "Appending differently tagged nodes"

-- | Type used to represent a directed graph
--
newtype DirectedGraph a = DirectedGraph {unDirectedGraph :: Map a (Node a)}
                        deriving (Show, Binary)

-- | Allow users to concatenate different graphs
--
instance Ord a => Monoid (DirectedGraph a) where
    mempty = DirectedGraph M.empty
    mappend (DirectedGraph m1) (DirectedGraph m2) = DirectedGraph $
        M.unionWith appendNodes m1 m2
