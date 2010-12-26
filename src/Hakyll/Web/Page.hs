-- | A page is an important concept in Hakyll: it has a body (usually of the
-- type 'String') and number of metadata fields. This type is used to represent
-- pages on your website.
--
module Hakyll.Web.Page
    ( Page (..)
    , toMap
    ) where

import Data.Map (Map)
import qualified Data.Map as M

import Hakyll.Core.Writable

-- | Type used to represent pages
--
data Page a = Page
    { pageMetadata :: Map String String
    , pageBody     :: a
    }

instance Functor Page where
    fmap f (Page m b) = Page m (f b)

instance Writable a => Writable (Page a) where
    write p (Page _ b) = write p b

-- | Convert a page to a map. The body will be placed in the @body@ key.
--
toMap :: Page String -> Map String String
toMap (Page m b) = M.insert "body" b m
