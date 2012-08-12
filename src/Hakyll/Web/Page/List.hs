-- | Provides an easy way to combine several pages in a list. The applications
-- are obvious:
--
-- * A post list on a blog
--
-- * An image list in a gallery
--
-- * A sitemap
--
module Hakyll.Web.Page.List
    ( setFieldPageList
    , pageListCompiler
    , chronological
    , recentFirst
    , sortByBaseName
    ) where

import Control.Arrow ((>>>), arr, (>>^))
import Data.List (sortBy)
import Data.Monoid (Monoid, mconcat)
import Data.Ord (comparing)
import System.FilePath (takeBaseName)

import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Web.Page
import Hakyll.Web.Page.Metadata
import Hakyll.Web.Template

-- | Set a field of a page to a listing of pages
--
setFieldPageList :: TemplateString a =>
                    ([Page a] -> [Page a])
                 -- ^ Determines list order
                 -> Identifier (Template a)
                 -- ^ Applied to every page
                 -> String
                 -- ^ Key indicating which field should be set
                 -> Pattern (Page a)
                 -- ^ Selects pages to include in the list
                 -> Compiler (Page a) (Page a)
                 -- ^ Compiler that sets the page list in a field
setFieldPageList sort template key pattern =
    requireAllA pattern $ setFieldA key $ pageListCompiler sort template >>^ tsToString

-- | Create a list of pages
--
pageListCompiler :: TemplateString a =>
                    ([Page a] -> [Page a])  -- ^ Determine list order
                 -> Identifier (Template a) -- ^ Applied to pages
                 -> Compiler [Page a] a     -- ^ Compiles page list
pageListCompiler sort template =
    arr sort >>> applyTemplateToList template >>> arr concatPages

-- | Apply a template to every page in a list
--
applyTemplateToList :: TemplateString a => Identifier (Template a)
                    -> Compiler [Page a] [Page a]
applyTemplateToList identifier =
    require identifier $ \posts template -> map (applyTemplate template) posts

-- | Concatenate the bodies of a page list
--
concatPages :: Monoid m => [Page m] -> m
concatPages = mconcat . map pageBody

-- | Sort pages chronologically. This function assumes that the pages have a
-- @year-month-day-title.extension@ naming scheme -- as is the convention in
-- Hakyll.
--
chronological :: [Page a] -> [Page a]
chronological = sortBy $ comparing $ takeBaseName . getField "path"

-- | The reverse of 'chronological'
--
recentFirst :: [Page a] -> [Page a]
recentFirst = reverse . chronological

-- | Deprecated, see 'chronological'
--
sortByBaseName :: [Page a] -> [Page a]
sortByBaseName = chronological
{-# DEPRECATED sortByBaseName "Use chronological" #-}
