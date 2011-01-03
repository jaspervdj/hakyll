-- | A module containing a box datatype representing a compiled item. This
-- item can be of any type, given that a few restrictions hold (e.g. we want
-- a 'Typeable' instance to perform type-safe casts).
--
{-# LANGUAGE ExistentialQuantification #-}
module Hakyll.Core.CompiledItem
    ( CompiledItem (..)
    , compiledItem
    , unCompiledItem
    ) where

import Data.Binary (Binary)
import Data.Typeable (Typeable, cast)

import Hakyll.Core.Writable

-- | Box type for a compiled item
--
data CompiledItem =  forall a.  (Binary a, Typeable a, Writable a)
                  => CompiledItem a

instance Writable CompiledItem where
    write p (CompiledItem x) = write p x

-- | Box a value into a 'CompiledItem'
--
compiledItem :: (Binary a, Typeable a, Writable a)
             => a
             -> CompiledItem
compiledItem = CompiledItem

-- | Unbox a value from a 'CompiledItem'
--
unCompiledItem :: (Binary a, Typeable a, Writable a)
               => CompiledItem
               -> a
unCompiledItem (CompiledItem x) = case cast x of
    Just x' -> x'
    Nothing -> error "unCompiledItem: Unsupported type"
