-- | Module exporting a function that works as a filter on a dependency graph.
-- Given a list of obsolete nodes, this filter will reduce the graph so it only
-- contains obsolete nodes and nodes that depend (directly or indirectly) on
-- obsolete nodes.
--
module Hakyll.Core.DirectedGraph.ObsoleteFilter
    ( filterObsolete
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Hakyll.Core.DirectedGraph
import qualified Hakyll.Core.DirectedGraph as DG

-- | Given a list of obsolete items, filter the dependency graph so it only
-- contains these items
--
filterObsolete :: Ord a
               => Set a            -- ^ Obsolete items
               -> DirectedGraph a  -- ^ Dependency graph
               -> DirectedGraph a  -- ^ Resulting dependency graph
filterObsolete obsolete graph =
    let reversed = DG.reverse graph
        allObsolete = S.unions $ map (flip reachableNodes reversed)
                               $ S.toList obsolete
    in DG.filter (`S.member` allObsolete) graph
