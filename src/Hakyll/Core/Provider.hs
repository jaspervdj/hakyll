--------------------------------------------------------------------------------
-- | This module provides an wrapper API around the file system which does some
-- caching.
module Hakyll.Core.Provider
    ( -- * Constructing resource providers
      Provider
    , newProvider

      -- * Querying resource properties
    , resourceList
    , resourceExists
    , resourceModified
    , resourceModificationTime

      -- * Access to raw resource content
    , resourceString
    , resourceLBS

      -- * Access to metadata and body content
    , resourceMetadata
    , resourceBody
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Internal
import qualified Hakyll.Core.Provider.MetadataCache as Internal
import           Hakyll.Core.Provider.Modified


--------------------------------------------------------------------------------
-- | Wrapper to ensure metadata cache is invalidated if necessary
resourceMetadata :: Provider -> Identifier -> IO Metadata
resourceMetadata rp r = do
    _ <- resourceModified rp r
    Internal.resourceMetadata rp r


--------------------------------------------------------------------------------
-- | Wrapper to ensure metadata cache is invalidated if necessary
resourceBody :: Provider -> Identifier -> IO String
resourceBody rp r = do
    _ <- resourceModified rp r
    Internal.resourceBody rp r
