--------------------------------------------------------------------------------
-- | An item is a combination of some content and its 'Identifier'. This way, we
-- can still use the 'Identifier' to access metadata.
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Core.Item
    ( Item (..)
    , itemSetBody
    , itemM
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>), (<*>))
import           Data.Binary            (Binary (..))
import           Data.Typeable          (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier


--------------------------------------------------------------------------------
data Item a = Item
    { itemIdentifier :: Identifier
    , itemBody       :: a
    } deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Functor Item where
    fmap f (Item i x) = Item i (f x)


--------------------------------------------------------------------------------
instance Binary a => Binary (Item a) where
    put (Item i x) = put i >> put x
    get            = Item <$> get <*> get


--------------------------------------------------------------------------------
itemSetBody :: a -> Item b -> Item a
itemSetBody x (Item i _) = Item i x


--------------------------------------------------------------------------------
itemM :: Monad m => (a -> m b) -> Item a -> m (Item b)
itemM f (Item i b) = f b >>= \b' -> return (Item i b')
