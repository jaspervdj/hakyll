-- | Dump a directed graph in dot format. Used for debugging purposes
--
module Hakyll.Core.DirectedGraph.Dot
    ( toDot
    , writeDot
    ) where

import Hakyll.Core.DirectedGraph
import qualified Data.Set as S

-- | Convert a directed graph into dot format for debugging purposes
--
toDot :: Ord a
      => (a -> String)    -- ^ Convert nodes to dot names
      -> DirectedGraph a  -- ^ Graph to dump
      -> String           -- ^ Resulting string
toDot showTag graph = unlines $ concat
    [ return "digraph dependencies {"
    , concatMap showNode (S.toList $ nodes graph)
    , return "}"
    ]
  where
    showNode node = map (showEdge node) $ S.toList $ neighbours node graph
    showEdge x y = "    \"" ++ showTag x ++ "\" -> \"" ++ showTag y ++ "\";"

-- | Write out the @.dot@ file to a given file path. See 'toDot' for more
-- information.
--
writeDot :: Ord a => FilePath -> (a -> String) -> DirectedGraph a -> IO ()
writeDot path showTag = writeFile path . toDot showTag
