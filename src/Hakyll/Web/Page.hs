-- | A page is an important concept in Hakyll: it has a body (usually of the
-- type 'String') and number of metadata fields. This type is used to represent
-- pages on your website.
--
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Web.Page
    ( Page (..)
    , toMap
    , pageRead
    ) where

import Control.Arrow ((>>^))

import Data.Map (Map)
import qualified Data.Map as M

import Hakyll.Core.Compiler
import Hakyll.Web.Page.Internal
import Hakyll.Web.Page.Read

-- | Convert a page to a map. The body will be placed in the @body@ key.
--
toMap :: Page String -> Map String String
toMap (Page m b) = M.insert "body" b m

-- | Read a page (do not render it)
--
pageRead :: Compiler a (Page String)
pageRead = getResourceString >>^ readPage
