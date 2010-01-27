-- | Module containing some specialized functions to deal with tags.
--   This Module follows certain conventions. Stick with them.
--
--   More concrete: all functions in this module assume that the tags are
--   located in the @tags@ field, and separated by commas. An example file
--   @foo.markdown@ could look like:
--
--   > ---
--   > author: Philip K. Dick
--   > title: Do androids dream of electric sheep?
--   > tags: future, science fiction, humanoid
--   > ---
--   > The novel is set in a post-apocalyptic near future, where the Earth and
--   > its populations have been damaged greatly by Nuclear...
--
--   All the following functions would work with such a format.
module Text.Hakyll.Tags
    ( readTagMap
    , renderTagCloud
    , renderTagLinks
    ) where

import qualified Data.Map as M
import Data.List (intercalate)
import Control.Monad (foldM)
import Control.Arrow (second)
import Control.Applicative ((<$>))
import System.FilePath ((</>))

import Text.Hakyll.Hakyll (Hakyll)
import Text.Hakyll.Context (ContextManipulation, changeValue)
import Text.Hakyll.Regex
import Text.Hakyll.Util
import Text.Hakyll.Page
import Text.Hakyll.Internal.Cache
import Text.Hakyll.Internal.Template

-- | Read a tag map. This creates a map from tags to page paths.
--
--   You also have to give a unique identifier for every tagmap. This is for
--   caching reasons, so the tagmap will be stored in
--   @_cache/_tagmap/identifier@.
readTagMap :: String -- ^ Unique identifier for the tagmap.
           -> [FilePath]
           -> Hakyll (M.Map String [FilePath])
readTagMap identifier paths = do
    isCacheMoreRecent' <- isCacheMoreRecent fileName paths
    if isCacheMoreRecent' then M.fromAscList <$> getFromCache fileName
                          else do tagMap <- readTagMap'
                                  storeInCache (M.toAscList tagMap) fileName
                                  return tagMap
  where
    fileName = "tagmaps" </> identifier

    readTagMap' = foldM addPaths M.empty paths
    addPaths current path = do
        page <- readPage path
        let tags = map trim $ splitRegex "," $ getValue "tags" page
        return $ foldr (flip (M.insertWith (++)) [path]) current tags

-- | Render a tag cloud.
renderTagCloud :: M.Map String [FilePath] -- ^ Map as produced by @readTagMap@.
               -> (String -> String) -- ^ Function to produce an url for a tag.
               -> Float -- ^ Smallest font size, in percent.
               -> Float -- ^ Biggest font size, in percent.
               -> String -- ^ Result of the render.
renderTagCloud tagMap urlFunction minSize maxSize =
    intercalate " " $ map renderTag tagCount
  where
    renderTag :: (String, Float) -> String
    renderTag (tag, count) = 
        finalSubstitute linkTemplate $ M.fromList [ ("size", sizeTag count)
                                                  , ("url", urlFunction tag)
                                                  , ("tag", tag)
                                                  ]
    linkTemplate =
        fromString "<a style=\"font-size: $size\" href=\"$url\">$tag</a>"

    sizeTag :: Float -> String
    sizeTag count = show size' ++ "%"
      where
        size' :: Int
        size' = floor $ minSize + relative count * (maxSize - minSize)

    minCount = minimum $ map snd tagCount
    maxCount = maximum $ map snd tagCount
    relative count = (count - minCount) / (maxCount - minCount)

    tagCount :: [(String, Float)]
    tagCount = map (second $ fromIntegral . length) $ M.toList tagMap

-- | Render all tags to links.
--   
--   On your site, it is nice if you can display the tags on a page, but
--   naturally, most people would expect these are clickable.
--
--   So, this function takes a function to produce an url for a given tag, and
--   applies it on all tags.
--
--   Note that it is your own responsibility to ensure a page with such an url
--   exists.
renderTagLinks :: (String -> String) -- ^ Function to produce an url for a tag.
               -> ContextManipulation
renderTagLinks urlFunction = changeValue "tags" renderTagLinks'
  where
    renderTagLinks' = intercalate ", "
                    . map ((\t -> link t $ urlFunction t) . trim)
                    . splitRegex ","
