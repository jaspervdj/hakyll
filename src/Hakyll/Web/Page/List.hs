-- TODO: Port
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

import Control.Arrow ((>>>), arr)
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
setFieldPageList :: ([Page String] -> [Page String])  
                 -- ^ Determines list order
                 -> Identifier Template               
                 -- ^ Applied to every page
                 -> String
                 -- ^ Key indicating which field should be set
                 -> Pattern (Page String)             
                 -- ^ Selects pages to include in the list
                 -> Compiler (Page String) (Page String)
                 -- ^ Compiler that sets the page list in a field
setFieldPageList sort template key pattern =
    requireAllA pattern $ setFieldA key $ pageListCompiler sort template

-- | Create a list of pages
--
pageListCompiler :: ([Page String] -> [Page String])  -- ^ Determine list order
                 -> Identifier Template               -- ^ Applied to pages
                 -> Compiler [Page String] String     -- ^ Compiles page list
pageListCompiler sort template =
    arr sort >>> applyTemplateToList template >>> arr concatPages

-- | Apply a template to every page in a list
--
applyTemplateToList :: Identifier Template
                    -> Compiler [Page String] [Page String]
applyTemplateToList identifier = require identifier $
    \posts template -> map (applyTemplateToPage template) posts

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
