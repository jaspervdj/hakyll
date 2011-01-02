-- | A page is an important concept in Hakyll: it has a body (usually of the
-- type 'String') and number of metadata fields. This type is used to represent
-- pages on your website.
--
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Web.Page
    ( Page (..)
    , addField
    , toMap
    , pageRead
    , addDefaultFields
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>^), (&&&), (>>>))
import System.FilePath (takeBaseName, takeDirectory)
import Data.Map (Map)
import qualified Data.Map as M

import Hakyll.Core.Identifier
import Hakyll.Core.Compiler
import Hakyll.Web.Page.Internal
import Hakyll.Web.Page.Read
import Hakyll.Web.Util.String

-- | Add a metadata field. If the field already exists, it is not overwritten.
--
addField :: String  -- ^ Key
         -> String  -- ^ Value
         -> Page a  -- ^ Page to add it to
         -> Page a  -- ^ Resulting page
addField k v (Page m b) = Page (M.insertWith (flip const) k v m) b

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
-- * @$root@
--
-- * @$title@
--
addDefaultFields :: Compiler (Page a) (Page a)
addDefaultFields =   (getRoute &&& id >>^ uncurry addRoute)
                 >>> (getIdentifier &&& id >>^ uncurry addIdentifier)
  where
    -- Add root and url, based on route
    addRoute Nothing  = id
    addRoute (Just r) = addField "url" (toUrl r)
                      . addField "root" (toSiteRoot $ toUrl r)

    -- Add title and category, based on identifier
    addIdentifier i = addField "title" (takeBaseName p)
                    . addField "category" (takeBaseName $ takeDirectory p)
      where
        p = toFilePath i
