--------------------------------------------------------------------------------
-- | Module exporting the simple 'Resource' type
module Hakyll.Core.Resource
    ( -- * Constructing and deconstructing resources
      Resource
    , resource
    , unResource

      -- * Conversions to and from identifiers
    , fromIdentifier
    , toIdentifier

      -- * TODO: Move me
    , Metadata
    ) where


--------------------------------------------------------------------------------
import           Data.Map               (Map)


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier


--------------------------------------------------------------------------------
-- | A resource
newtype Resource = Resource {unResource :: FilePath}
    deriving (Eq, Show, Ord)


--------------------------------------------------------------------------------
-- | Smart constructor to ensure we have @/@ as path separator
resource :: FilePath -> Resource
resource = fromIdentifier . parseIdentifier


--------------------------------------------------------------------------------
-- | Find the resource for an identifier
fromIdentifier :: Identifier a -> Resource
fromIdentifier = Resource . toFilePath


--------------------------------------------------------------------------------
-- | Convert a resource to an identifier
toIdentifier :: Resource -> Identifier a
toIdentifier = parseIdentifier . unResource


--------------------------------------------------------------------------------
type Metadata = Map String String
