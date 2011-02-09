-- | Internal representation of the page datatype
--
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Web.Page.Internal
    ( Page (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid, mempty, mappend)

import Data.Map (Map)
import Data.Binary (Binary, get, put)
import Data.Typeable (Typeable)
import qualified Data.Map as M

import Hakyll.Core.Writable

-- | Type used to represent pages
--
data Page a = Page
    { pageMetadata :: Map String String
    , pageBody     :: a
    } deriving (Eq, Show, Typeable)

instance Monoid a => Monoid (Page a) where
    mempty = Page M.empty mempty
    mappend (Page m1 b1) (Page m2 b2) =
        Page (M.union m1 m2) (mappend b1 b2)

instance Functor Page where
    fmap f (Page m b) = Page m (f b)

instance Binary a => Binary (Page a) where
    put (Page m b) = put m >> put b
    get = Page <$> get <*> get

instance Writable a => Writable (Page a) where
    write p (Page _ b) = write p b
