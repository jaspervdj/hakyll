--------------------------------------------------------------------------------
-- | Module containing some specialized functions to deal with tags.
-- This Module follows certain conventions. My advice is to stick with them if
-- possible.
--
-- More concrete: all functions in this module assume that the tags are
-- located in the @tags@ field, and separated by commas. An example file
-- @foo.markdown@ could look like:
--
-- > ---
-- > author: Philip K. Dick
-- > title: Do androids dream of electric sheep?
-- > tags: future, science fiction, humanoid
-- > ---
-- > The novel is set in a post-apocalyptic near future, where the Earth and
-- > its populations have been damaged greatly by Nuclear...
--
-- All the following functions would work with such a format. In addition to
-- tags, Hakyll also supports categories. The convention when using categories
-- is to place pages in subdirectories.
--
-- An example, the page @posts\/coding\/2010-01-28-hakyll-categories.markdown@
-- Tags or categories are read using the @buildTags@ and @buildCategory@
-- functions. This module only provides functions to work with tags:
-- categories are represented as tags. This is perfectly possible: categories
-- only have an additional restriction that a page can only have one category
-- (instead of multiple tags).
{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Web.Tags
    ( Tags (..)
    , getTags
    , buildTagsWith
    , buildTags
    , buildCategory
    , renderTagCloud
    , renderTagList
    , tagsField
    , categoryField
    , sortTagsBy
    , caseInsensitiveTags
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Control.Monad                   (foldM, forM)
import           Data.Binary                     (Binary)
import           Data.Char                       (toLower)
import           Data.List                       (intercalate, intersperse,
                                                  sortBy)
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Monoid                     (mconcat)
import           Data.Ord                        (comparing)
import           Data.Typeable                   (Typeable)
import           System.FilePath                 (takeBaseName, takeDirectory)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Util.String
import           Hakyll.Core.Writable
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Urls


--------------------------------------------------------------------------------
-- | Data about tags
-- TODO Make this a map instead of a list?
newtype Tags = Tags
    { unTags :: [(String, [Identifier])]
    } deriving (Binary, Show, Typeable)


--------------------------------------------------------------------------------
instance Writable Tags where
    write _ _ = return ()


--------------------------------------------------------------------------------
-- | Obtain tags from a page in the default way: parse them from the @tags@
-- metadata field.
getTags :: MonadMetadata m => Identifier -> m [String]
getTags identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll ",") $ M.lookup "tags" metadata

--------------------------------------------------------------------------------
-- | Obtain categories from a page.
getCategory :: MonadMetadata m => Identifier -> m [String]
getCategory = return . return . takeBaseName . takeDirectory . toFilePath


--------------------------------------------------------------------------------
-- | Higher-order function to read tags
buildTagsWith :: MonadMetadata m
              => (Identifier -> m [String]) -> Pattern -> m Tags
buildTagsWith f pattern = do
    ids    <- getMatches pattern
    tagMap <- foldM addTags M.empty ids
    return $ Tags $ M.toList tagMap
  where
    -- Create a tag map for one page
    addTags tagMap id' = do
        tags <- f id'
        let tagMap' = M.fromList $ zip tags $ repeat [id']
        return $ M.unionWith (++) tagMap tagMap'

--------------------------------------------------------------------------------
-- | Read a tagmap using the @tags@ metadata field
buildTags :: MonadMetadata m => Pattern -> m Tags
buildTags = buildTagsWith getTags


--------------------------------------------------------------------------------
-- | Read a tagmap using the @category@ metadata field
buildCategory :: MonadMetadata m => Pattern -> m Tags
buildCategory = buildTagsWith getCategory


--------------------------------------------------------------------------------
-- | Render tags in HTML
renderTags :: (String -> Identifier)
           -- ^ Produce a tag page id
           -> (String -> String -> Int -> Int -> Int -> String)
           -- ^ Produce a tag item: tag, url, count, min count, max count
           -> ([String] -> String)
           -- ^ Join items
           -> Tags
           -- ^ Tag cloud renderer
           -> Compiler String
renderTags makeTagId makeHtml concatHtml (Tags tags) = do
    -- In tags' we create a list: [((tag, route), count)]
    tags' <- forM tags $ \(tag, ids) -> do
        route <- getRoute $ makeTagId tag
        return ((tag, route), length ids)

    let -- Absolute frequencies of the pages
        freqs = map snd tags'

        -- The minimum and maximum count found
        (min', max')
            | null freqs = (0, 1)
            | otherwise  = (minimum &&& maximum) freqs

        -- Create a link for one item
        makeHtml' ((tag, url), count) =
            makeHtml tag (toUrl $ fromMaybe "/" url) count min' max'

    -- Render and return the HTML
    return $ concatHtml $ map makeHtml' tags'


--------------------------------------------------------------------------------
-- | Render a tag cloud in HTML
-- TODO: Maybe produce a Context here
renderTagCloud :: (String -> Identifier)
               -- ^ Produce a link for a tag
               -> Double
               -- ^ Smallest font size, in percent
               -> Double
               -- ^ Biggest font size, in percent
               -> Tags
               -- ^ Input tags
               -> Compiler String
               -- ^ Rendered cloud
renderTagCloud makeTagId minSize maxSize =
    renderTags makeTagId makeLink (intercalate " ")
  where
    makeLink tag url count min' max' = renderHtml $
        H.a ! A.style (toValue $ "font-size: " ++ size count min' max')
            ! A.href (toValue url)
            $ toHtml tag

    -- Show the relative size of one 'count' in percent
    size count min' max' =
        let diff = 1 + fromIntegral max' - fromIntegral min'
            relative = (fromIntegral count - fromIntegral min') / diff
            size' = floor $ minSize + relative * (maxSize - minSize)
        in show (size' :: Int) ++ "%"


--------------------------------------------------------------------------------
-- | Render a simple tag list in HTML, with the tag count next to the item
-- TODO: Maybe produce a Context here
renderTagList :: (String -> Identifier) -> Tags -> Compiler (String)
renderTagList makeTagId = renderTags makeTagId makeLink (intercalate ", ")
  where
    makeLink tag url count _ _ = renderHtml $
        H.a ! A.href (toValue url) $ toHtml (tag ++ " (" ++ show count ++ ")")


--------------------------------------------------------------------------------
-- | Render tags with links with custom function to get tags. It is typically
-- together with 'getTags' like this:
--
-- > renderTagsFieldWith (customFunction . getTags)
-- >     "tags" (fromCapture "tags/*")
tagsFieldWith :: (Identifier -> Compiler [String])  -- ^ Get the tags
              -> String                             -- ^ Destination key
              -> (String -> Identifier)             -- ^ Create a link for a tag
              -> Context a                          -- ^ Resulting context
tagsFieldWith getTags' key makeTagId = field key $ \item -> do
    tags  <- getTags' $ itemIdentifier item
    links <- forM tags $ \tag -> do
        route <- getRoute $ makeTagId tag
        return $ renderLink tag route

    return $ renderHtml $ mconcat $ intersperse ", " $ catMaybes $ links
  where
    -- Render one tag link
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
        H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag


--------------------------------------------------------------------------------
-- | Render tags with links
tagsField :: String                  -- ^ Destination key
          -> (String -> Identifier)  -- ^ Create a link for a tag
          -> Context a               -- ^ Context
tagsField = tagsFieldWith getTags


--------------------------------------------------------------------------------
-- | Render the category in a link
categoryField :: String                  -- ^ Destination key
              -> (String -> Identifier)  -- ^ Create a category link
              -> Context a               -- ^ Context
categoryField = tagsFieldWith getCategory


--------------------------------------------------------------------------------
-- | Sort tags using supplied function. First element of the tuple passed to
-- the comparing function is the actual tag name.
sortTagsBy :: ((String, [Identifier]) -> (String, [Identifier]) -> Ordering)
           -> Tags -> Tags
sortTagsBy f = Tags . sortBy f . unTags


--------------------------------------------------------------------------------
-- | Sample sorting function that compares tags case insensitively.
caseInsensitiveTags :: (String, [Identifier]) -> (String, [Identifier])
                    -> Ordering
caseInsensitiveTags = comparing $ map toLower . fst
