--------------------------------------------------------------------------------
-- | This module provides an wrapper API around the file system which does some
-- caching.
module Hakyll.Core.ResourceProvider
    ( -- * Constructing resource providers
      ResourceProvider
    , newResourceProvider

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
import           Hakyll.Core.ResourceProvider.Internal
import qualified Hakyll.Core.ResourceProvider.MetadataCache as Internal
import           Hakyll.Core.ResourceProvider.Modified


--------------------------------------------------------------------------------
-- | Wrapper to ensure metadata cache is invalidated if necessary
resourceMetadata :: ResourceProvider -> Identifier a -> IO Metadata
resourceMetadata rp r = do
    _ <- resourceModified rp r
    Internal.resourceMetadata rp r


--------------------------------------------------------------------------------
-- | Wrapper to ensure metadata cache is invalidated if necessary
resourceBody :: ResourceProvider -> Identifier a -> IO String
resourceBody rp r = do
    _ <- resourceModified rp r
    Internal.resourceBody rp r
