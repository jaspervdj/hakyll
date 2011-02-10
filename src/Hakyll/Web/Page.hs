-- | A page is an important concept in Hakyll: it has a body (usually of the
-- type 'String') and number of metadata fields. This type is used to represent
-- pages on your website.
--
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Web.Page
    ( Page (..)
    , fromBody
    , fromMap
    , toMap
    , pageRead
    , addDefaultFields
    , sortByBaseName
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>^), (&&&), (>>>))
import System.FilePath (takeBaseName, takeDirectory)
import Data.Monoid (Monoid, mempty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Ord (comparing)

import Hakyll.Core.Identifier
import Hakyll.Core.Compiler
import Hakyll.Web.Page.Internal
import Hakyll.Web.Page.Read
import Hakyll.Web.Page.Metadata
import Hakyll.Web.Util.String

-- | Create a page from a body, without metadata
--
fromBody :: a -> Page a
fromBody = Page M.empty

-- | Create a metadata page, without a body
--
fromMap :: Monoid a => Map String String -> Page a
fromMap m = Page m mempty

-- | Convert a page to a map. The body will be placed in the @body@ key.
--
toMap :: Page String -> Map String String
toMap (Page m b) = M.insert "body" b m

-- | Read a page (do not render it)
--
pageRead :: Compiler a (Page String)
pageRead = getResourceString >>^ readPage

-- | Add a number of default metadata fields to a page. These fields include:
--
-- * @$url@
--
-- * @$category@
--
-- * @$title@
--
-- * @$path@
--
addDefaultFields :: Compiler (Page a) (Page a)
addDefaultFields =   (getRoute &&& id >>^ uncurry addRoute)
                 >>> (getIdentifier &&& id >>^ uncurry addIdentifier)
  where
    -- Add root and url, based on route
    addRoute Nothing  = id
    addRoute (Just r) = setField "url" (toUrl r)

    -- Add title and category, based on identifier
    addIdentifier i = setField "title" (takeBaseName p)
                    . setField "category" (takeBaseName $ takeDirectory p)
                    . setField "path" p
      where
        p = toFilePath i

-- | Sort posts based on the basename of the post. This is equivalent to a
-- chronologival sort, because of the @year-month-day-title.extension@ naming
-- convention in Hakyll.
--
sortByBaseName :: [Page a] -> [Page a]
sortByBaseName = sortBy $ comparing $ takeBaseName . getField "path"
