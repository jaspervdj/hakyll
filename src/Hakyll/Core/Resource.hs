-- | Module exporting the simple 'Resource' type
--
module Hakyll.Core.Resource
    ( Resource (..)
    , fromIdentifier
    , toIdentifier
    ) where

import Hakyll.Core.Identifier

-- | A resource
--
newtype Resource = Resource {unResource :: String}
                 deriving (Eq, Show, Ord)

-- | Create a resource from an identifier
--
fromIdentifier :: Identifier a -> Resource
fromIdentifier = Resource . toFilePath

-- | Map the resource to an identifier. Note that the group will not be set!
--
toIdentifier :: Resource -> Identifier a
toIdentifier = parseIdentifier . unResource
