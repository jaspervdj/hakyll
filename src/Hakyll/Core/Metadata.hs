--------------------------------------------------------------------------------
module Hakyll.Core.Metadata
    ( Metadata
    , MonadMetadata (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Map               (Map)


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier


--------------------------------------------------------------------------------
type Metadata = Map String String


--------------------------------------------------------------------------------
class MonadMetadata m where
    identifierMetadata :: Identifier -> m Metadata
    -- allMetadata :: m [(Resource, Metadata)]
    -- patternMetadata :: Pattern a -> m [(Resource, Metadata)]
