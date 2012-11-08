--------------------------------------------------------------------------------
-- | This module provides an wrapper API around the file system which does some
-- caching.
module Hakyll.Core.Resource.Provider
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
import           Hakyll.Core.Resource
import qualified Hakyll.Core.Resource.MetadataCache     as Internal
import           Hakyll.Core.Resource.Modified
import           Hakyll.Core.Resource.Provider.Internal


--------------------------------------------------------------------------------
-- | Wrapper to ensure metadata cache is invalidated if necessary
resourceMetadata :: ResourceProvider -> Resource -> IO Metadata
resourceMetadata rp r = do
    _ <- resourceModified rp r
    Internal.resourceMetadata rp r


--------------------------------------------------------------------------------
-- | Wrapper to ensure metadata cache is invalidated if necessary
resourceBody :: ResourceProvider -> Resource -> IO String
resourceBody rp r = do
    _ <- resourceModified rp r
    Internal.resourceBody rp r
