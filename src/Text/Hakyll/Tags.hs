-- | Module containing some specialized functions to deal with tags.
--   This Module follows certain conventions. Stick with them.
module Text.Hakyll.Tags
    ( readTagMap
    , renderTagCloud
    , renderTagLinks
    ) where

import qualified Data.Map as M
import Data.List (intercalate)
import Control.Monad (foldM)
import Text.Hakyll.Hakyll (Hakyll)

import Text.Hakyll.Context (ContextManipulation, renderValue)
import Text.Hakyll.Regex
import Text.Hakyll.Util
import Text.Hakyll.Page
import Control.Arrow (second)

-- | Read a tag map. This creates a map from tags to page paths. This function
--   assumes the tags are located in the `tags` metadata field, separated by
--   commas.
readTagMap :: [FilePath] -> Hakyll (M.Map String [FilePath])
readTagMap paths = foldM addPaths M.empty paths
  where
    addPaths current path = do
        page <- readPage path
        let tags = map trim $ splitRegex "," $ getValue ("tags") page
        return $ foldr (\t -> M.insertWith (++) t [path]) current tags

-- | Render a tag cloud.
renderTagCloud :: M.Map String [FilePath] -- ^ A tag map as produced by 'readTagMap'.
               -> (String -> String) -- ^ Function that produces an url for a tag.
               -> Float -- ^ Smallest font size, in percent.
               -> Float -- ^ Biggest font size, in percent.
               -> String -- ^ Result of the render.
renderTagCloud tagMap urlFunction minSize maxSize =
    intercalate " " $ map renderTag tagCount
  where
    renderTag :: (String, Float) -> String
    renderTag (tag, count) =  "<a style=\"font-size: "
                           ++ sizeTag count ++ "\" href=\""
                           ++ urlFunction tag ++ "\">"
                           ++ tag ++ "</a>"

    sizeTag :: Float -> String
    sizeTag count = show size' ++ "%"
      where
        size' :: Int
        size' = floor (minSize + (relative count) * (maxSize - minSize))

    minCount = minimum $ map snd $ tagCount
    maxCount = maximum $ map snd $ tagCount
    relative count = (count - minCount) / (maxCount - minCount)

    tagCount :: [(String, Float)]
    tagCount = map (second $ fromIntegral . length) $ M.toList tagMap

-- Render all tags to links.
renderTagLinks :: (String -> String) -- ^ Function that produces an url for a tag.
               -> ContextManipulation
renderTagLinks urlFunction = renderValue "tags" "tags" renderTagLinks'
  where
    renderTagLinks' = intercalate ", "
                    . map (\t -> link t $ urlFunction t)
                    . map trim . splitRegex ","
