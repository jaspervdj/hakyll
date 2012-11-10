--------------------------------------------------------------------------------
-- | Representation of a directed graph. In Hakyll, this is used for dependency
-- tracking.
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.DirectedGraph
    ( DirectedGraph

    , fromList
    , toList

    , member
    , nodes
    , neighbours
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow   (second)
import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     (Monoid (..))
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Data.Typeable   (Typeable)
import           Prelude         hiding (reverse)


--------------------------------------------------------------------------------
-- | Type used to represent a directed graph
newtype DirectedGraph a = DirectedGraph {unDirectedGraph :: Map a [a]}
    deriving (Show, Binary, NFData, Typeable)


--------------------------------------------------------------------------------
-- | Allow users to concatenate different graphs
instance Ord a => Monoid (DirectedGraph a) where
    mempty                                        = DirectedGraph M.empty
    mappend (DirectedGraph m1) (DirectedGraph m2) = DirectedGraph $
        M.unionWith (\x y -> sortUnique (x ++ y)) m1 m2


--------------------------------------------------------------------------------
-- | Construction of directed graphs
fromList :: Ord a
         => [(a, [a])]       -- ^ List of (node, reachable neighbours)
         -> DirectedGraph a  -- ^ Resulting directed graph
fromList = DirectedGraph . M.fromList . map (second sortUnique)


--------------------------------------------------------------------------------
-- | Deconstruction of directed graphs
toList :: DirectedGraph a
       -> [(a, [a])]
toList = M.toList . unDirectedGraph


--------------------------------------------------------------------------------
-- | Check if a node lies in the given graph
member :: Ord a
       => a                -- ^ Node to check for
       -> DirectedGraph a  -- ^ Directed graph to check in
       -> Bool             -- ^ If the node lies in the graph
member n = M.member n . unDirectedGraph


--------------------------------------------------------------------------------
-- | Get all nodes in the graph
nodes :: Ord a
      => DirectedGraph a  -- ^ Graph to get the nodes from
      -> Set a            -- ^ All nodes in the graph
nodes = M.keysSet . unDirectedGraph


--------------------------------------------------------------------------------
-- | Get a set of reachable neighbours from a directed graph
neighbours :: Ord a
           => a                -- ^ Node to get the neighbours of
           -> DirectedGraph a  -- ^ Graph to search in
           -> [a]              -- ^ Set containing the neighbours
neighbours x dg = fromMaybe [] $ M.lookup x $ unDirectedGraph dg


--------------------------------------------------------------------------------
-- | Sort and make unique
sortUnique :: Ord a => [a] -> [a]
sortUnique = S.toAscList . S.fromList
