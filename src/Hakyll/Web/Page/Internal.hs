-- | Internal representation of the page datatype
--
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Web.Page.Internal
    ( Page (..)
    ) where

import Control.Applicative ((<$>), (<*>))

import Data.Map (Map)
import Data.Binary (Binary, get, put)
import Data.Typeable (Typeable)

import Hakyll.Core.Writable

-- | Type used to represent pages
--
data Page a = Page
    { pageMetadata :: Map String String
    , pageBody     :: a
    } deriving (Show, Typeable)

instance Functor Page where
    fmap f (Page m b) = Page m (f b)

instance Binary a => Binary (Page a) where
    put (Page m b) = put m >> put b
    get = Page <$> get <*> get

instance Writable a => Writable (Page a) where
    write p (Page _ b) = write p b
