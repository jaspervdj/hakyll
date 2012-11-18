--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
module Hakyll.Core.Item.SomeItem
    ( SomeItem (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Binary          (Binary)
import           Data.Typeable        (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Item
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | An existential type, mostly for internal usage.
data SomeItem = forall a.
    (Binary a, Typeable a, Writable a) => SomeItem (Item a)
    deriving (Typeable)
