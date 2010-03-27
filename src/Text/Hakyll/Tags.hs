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
--   All the following functions would work with such a format. In addition to
--   tags, Hakyll also supports categories. The convention when using categories
--   is to place pages in subdirectories.
--
--   An example, the page @posts\/coding\/2010-01-28-hakyll-categories.markdown@
--   Tags or categories are read using the @readTagMap@ and @readCategoryMap@
--   functions. Because categories are implemented using tags - categories can
--   be seen as tags, with the restriction that a page can only have one
--   category - all functions for tags also work with categories.
--
--   When reading a @TagMap@ (which is also used for category maps) using the
--   @readTagMap@ or @readCategoryMap@ function, you also have to give a unique
--   identifier to it. This identifier is simply for caching reasons, so Hakyll
--   can tell different maps apart; it has no other use.
module Text.Hakyll.Tags
    ( TagMap
    , readTagMap
    , readCategoryMap
    , withTagMap
    , renderTagCloud
    , renderTagLinks
    ) where

import qualified Data.Map as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Arrow (second, (>>>))
import Control.Applicative ((<$>))
import System.FilePath

import Text.Hakyll.Context (Context)
import Text.Hakyll.ContextManipulations (changeValue)
import Text.Hakyll.CreateContext (createPage)
import Text.Hakyll.HakyllMonad (Hakyll)
import Text.Hakyll.Regex
import Text.Hakyll.HakyllAction
import Text.Hakyll.Util
import Text.Hakyll.Internal.Cache
import Text.Hakyll.Internal.Template

-- | Type for a tag map.
--
--   This is a map associating tags or categories to the appropriate pages
--   using that tag or category. In the case of categories, each path will only
--   appear under one category - this is not the case with tags.
type TagMap = M.Map String [HakyllAction () Context]

-- | Read a tag map. This is a internally used function that can be used for
--   tags as well as for categories.
readMap :: (Context -> [String]) -- ^ Function to get tags from a context.
        -> String -- ^ Unique identifier for the tagmap.
        -> [FilePath]
        -> HakyllAction () TagMap
readMap getTagsFunction identifier paths = HakyllAction
    { actionDependencies = paths
    , actionUrl          = Nothing
    , actionFunction     = actionFunction'
    } 
  where
    fileName = "tagmaps" </> identifier

    actionFunction' _ = do
        isCacheMoreRecent' <- isCacheMoreRecent fileName paths
        assocMap <- if isCacheMoreRecent'
                        then M.fromAscList <$> getFromCache fileName
                        else do assocMap' <- readTagMap'
                                storeInCache (M.toAscList assocMap') fileName
                                return assocMap'
        return $ M.map (map createPage) assocMap

    -- TODO: preserve order
    readTagMap' :: Hakyll (M.Map String [FilePath])
    readTagMap' = do
        pairs' <- concat <$> mapM pairs paths
        return $ M.fromListWith (flip (++)) pairs'

    -- | Read a page, and return an association list where every tag is
    -- associated with some paths. Of course, this will always be just one
    -- @FilePath@ here.
    pairs :: FilePath -> Hakyll [(String, [FilePath])]
    pairs path = do
        context <- runHakyllAction $ createPage path
        let tags = getTagsFunction context
        return $ map (\tag -> (tag, [path])) tags

-- | Read a @TagMap@, using the @tags@ metadata field.
readTagMap :: String     -- ^ Unique identifier for the map.
           -> [FilePath] -- ^ Paths to get tags from.
           -> HakyllAction () TagMap
readTagMap = readMap getTagsFunction
  where
    getTagsFunction = map trim . splitRegex ","
                    . fromMaybe [] . M.lookup "tags"

-- | Read a @TagMap@, using the subdirectories the pages are placed in.
readCategoryMap :: String     -- ^ Unique identifier for the map.
                -> [FilePath] -- ^ Paths to get tags from.
                -> HakyllAction () TagMap
readCategoryMap = readMap $ maybeToList . M.lookup "category"

withTagMap :: HakyllAction () TagMap
           -> (String -> [HakyllAction () Context] -> Hakyll ())
           -> Hakyll ()
withTagMap tagMap function = runHakyllAction (tagMap >>> action)
  where
    action = createHakyllAction (mapM_ (uncurry function) . M.toList)

-- | Render a tag cloud.
renderTagCloud :: (String -> String) -- ^ Function to produce an url for a tag.
               -> Float              -- ^ Smallest font size, in percent.
               -> Float              -- ^ Biggest font size, in percent.
               -> HakyllAction TagMap String
renderTagCloud urlFunction minSize maxSize = createHakyllAction renderTagCloud'
  where
    renderTagCloud' tagMap =
        return $ intercalate " " $ map (renderTag tagMap) (tagCount tagMap)

    renderTag tagMap (tag, count) = 
        finalSubstitute linkTemplate $ M.fromList
            [ ("size", sizeTag tagMap count)
            , ("url", urlFunction tag)
            , ("tag", tag)
            ]

    linkTemplate =
        fromString "<a style=\"font-size: $size\" href=\"$url\">$tag</a>"

    sizeTag tagMap count = show (size' :: Int) ++ "%"
      where
        size' = floor $ minSize + relative tagMap count * (maxSize - minSize)

    minCount = minimum . map snd . tagCount
    maxCount = maximum . map snd . tagCount
    relative tagMap count = (count - minCount tagMap) /
                            (maxCount tagMap - minCount tagMap)

    tagCount = map (second $ fromIntegral . length) . M.toList

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
               -> HakyllAction Context Context
renderTagLinks urlFunction = changeValue "tags" renderTagLinks'
  where
    renderTagLinks' = intercalate ", "
                    . map ((\t -> link t $ urlFunction t) . trim)
                    . splitRegex ","
