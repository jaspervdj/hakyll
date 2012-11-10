--------------------------------------------------------------------------------
-- | Dump a directed graph in dot format. Used for debugging purposes
module Hakyll.Core.DirectedGraph.Dot
    ( toDot
    , writeDot
    ) where


--------------------------------------------------------------------------------
import qualified Data.Set                  as S
import           Hakyll.Core.DirectedGraph


--------------------------------------------------------------------------------
-- | Convert a directed graph into dot format for debugging purposes
toDot :: Ord a
      => (a -> String)    -- ^ Convert nodes to dot names
      -> DirectedGraph a  -- ^ Graph to dump
      -> String           -- ^ Resulting string
toDot showTag graph = unlines $ concat
    [ return "digraph dependencies {"
    , map showNode (S.toList $ nodes graph)
    , concatMap showEdges (S.toList $ nodes graph)
    , return "}"
    ]
  where
    showNode node = "    \"" ++ showTag node ++ "\";"
    showEdges node = map (showEdge node) $ neighbours node graph
    showEdge x y = "    \"" ++ showTag x ++ "\" -> \"" ++ showTag y ++ "\";"


--------------------------------------------------------------------------------
-- | Write out the @.dot@ file to a given file path. See 'toDot' for more
-- information.
writeDot :: Ord a => FilePath -> (a -> String) -> DirectedGraph a -> IO ()
writeDot path showTag = writeFile path . toDot showTag
