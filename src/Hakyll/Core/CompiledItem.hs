--------------------------------------------------------------------------------
-- | A module containing a box datatype representing a compiled item. This
-- item can be of any type, given that a few restrictions hold:
--
-- * we need a 'Typeable' instance to perform type-safe casts;
--
-- * we need a 'Binary' instance so we can serialize these items to the cache;
--
-- * we need a 'Writable' instance so the results can be saved.
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
module Hakyll.Core.CompiledItem
    ( CompiledItem (..)
    , compiledItem
    , unCompiledItem
    ) where


--------------------------------------------------------------------------------
import           Data.Binary          (Binary)
import           Data.Maybe           (fromMaybe)
import           Data.Typeable        (Typeable, cast, typeOf)


--------------------------------------------------------------------------------
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | Box type for a compiled item
--
data CompiledItem =  forall a.  (Binary a, Typeable a, Writable a)
                  => CompiledItem a
                  deriving (Typeable)


--------------------------------------------------------------------------------
instance Writable CompiledItem where
    write p (CompiledItem x) = write p x


--------------------------------------------------------------------------------
-- | Box a value into a 'CompiledItem'
compiledItem :: (Binary a, Typeable a, Writable a) => a -> CompiledItem
compiledItem = CompiledItem


--------------------------------------------------------------------------------
-- | Unbox a value from a 'CompiledItem'
unCompiledItem :: (Binary a, Typeable a, Writable a) => CompiledItem -> a
unCompiledItem (CompiledItem x) = fromMaybe error' $ cast x
  where
    error' = error $
        "Hakyll.Core.CompiledItem.unCompiledItem: " ++
        "unsupported type (got " ++ show (typeOf x) ++ ")"
