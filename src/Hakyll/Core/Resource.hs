-- | Module exporting the simple 'Resource' type
--
module Hakyll.Core.Resource
    ( Resource
    , unResource
    , resource
    , fromIdentifier
    , toIdentifier
    ) where

import Hakyll.Core.Identifier

-- | A resource
--
newtype Resource = Resource {unResource :: FilePath}
    deriving (Eq, Show, Ord)

-- | Smart constructor to ensure we have @/@ as path separator
--
resource :: FilePath -> Resource
resource = fromIdentifier . parseIdentifier

-- | Create a resource from an identifier
--
fromIdentifier :: Identifier a -> Resource
fromIdentifier = Resource . toFilePath

-- | Map the resource to an identifier. Note that the group will not be set!
--
toIdentifier :: Resource -> Identifier a
toIdentifier = parseIdentifier . unResource
