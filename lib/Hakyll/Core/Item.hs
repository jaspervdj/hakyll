--------------------------------------------------------------------------------
-- | An item is a combination of some content and its 'Identifier'. This way, we
-- can still use the 'Identifier' to access metadata.
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Core.Item
    ( Item (..)
    , itemSetBody
    , withItemBody
    ) where


--------------------------------------------------------------------------------
import           Data.Binary                   (Binary (..))
import           Data.Foldable                 (Foldable (..))
import           Data.Typeable                 (Typeable)
import           Prelude                       hiding (foldr)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
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
instance Foldable Item where
    foldr f z (Item _ x) = f x z


--------------------------------------------------------------------------------
instance Traversable Item where
    traverse f (Item i x) = Item i <$> f x


--------------------------------------------------------------------------------
instance Binary a => Binary (Item a) where
    put (Item i x) = put i >> put x
    get            = Item <$> get <*> get


--------------------------------------------------------------------------------
itemSetBody :: a -> Item b -> Item a
itemSetBody x (Item i _) = Item i x


--------------------------------------------------------------------------------
-- | Perform a compiler action on the item body. This is the same as 'traverse',
-- but looks less intimidating.
--
-- > withItemBody = traverse
withItemBody :: (a -> Compiler b) -> Item a -> Compiler (Item b)
withItemBody = traverse
