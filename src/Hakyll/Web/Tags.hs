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
-- Tags or categories are read using the @readTags@ and @readCategories@
-- functions. This module only provides functions to work with tags:
-- categories are represented as tags. This is perfectly possible: categories
-- only have an additional restriction that a page can only have one category
-- (instead of multiple tags).
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Hakyll.Web.Tags
    ( Tags (..)
    , readTagsWith
    , readTags
    , readCategories
    , renderTagCloud
    ) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (intersperse)
import Control.Arrow (second, (&&&))

import Data.Typeable (Typeable)
import Data.Binary (Binary, get, put)
import Data.Monoid (mconcat)
import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze (Html, (!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hakyll.Web.Page
import Hakyll.Web.Page.Metadata
import Hakyll.Web.Util.String
import Hakyll.Core.Writable

-- | Data about tags
--
data Tags a = Tags
    { tagsMap :: Map String [Page a]
    } deriving (Show, Typeable)

instance Binary a => Binary (Tags a) where
    get = Tags <$> get
    put (Tags m) = put m

instance Writable (Tags a) where
    write _ _ = return ()

-- | Higher-level function to read tags
--
readTagsWith :: (Page a -> [String])  -- ^ Function extracting tags from a page
             -> [Page a]              -- ^ Pages
             -> Tags a                -- ^ Resulting tags
readTagsWith f pages = Tags
    { tagsMap = foldl (M.unionWith (++)) M.empty (map readTagsWith' pages)
    }
  where
    -- Create a tag map for one page
    readTagsWith' page =
        let tags = f page
        in M.fromList $ zip tags $ repeat [page]

-- | Read a tagmap using the @tags@ metadata field
--
readTags :: [Page a] -> Tags a
readTags = readTagsWith $ map trim . splitAll "," . getField "tags"

-- | Read a tagmap using the @category@ metadata field
--
readCategories :: [Page a] -> Tags a
readCategories = readTagsWith $ return . getField "category"

-- | Render a tag cloud in HTML
--
renderTagCloud :: (String -> String)  -- ^ Function to produce an url for a tag
               -> Double              -- ^ Smallest font size, in percent
               -> Double              -- ^ Biggest font size, in percent
               -> Tags a              -- ^ Tags structure to render
               -> String              -- ^ Resulting HTML
renderTagCloud urlFunction minSize maxSize (Tags tags) = renderHtml $
    mconcat $ intersperse " " $ map (uncurry renderTag) withCount
  where
    -- Tags composed with their count
    withCount = map (second $ fromIntegral . length) $ M.toList tags

    -- Render one tag, given it's count
    renderTag :: String -> Int -> Html
    renderTag tag count =
        H.a ! A.style (toValue $ "font-size: " ++ size count)
            ! A.href (toValue $ urlFunction tag)
            $ toHtml tag

    -- Show the relative size of one 'count' in percent
    size count =
        let size' = floor $ minSize + relative count * (maxSize - minSize)
        in show (size' :: Int) ++ "%"

    -- Find out the relative count of a tag: on a scale from 0 to 1
    relative count = (fromIntegral count - minCount) / (1 + maxCount - minCount)

    -- The minimum and maximum count found, as doubles
    (minCount, maxCount)
        | null withCount = (0, 1)
        | otherwise = (minimum &&& maximum) $ map (fromIntegral . snd) withCount
