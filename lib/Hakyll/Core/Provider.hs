--------------------------------------------------------------------------------
-- | This module provides an wrapper API around the file system which does some
-- caching.
module Hakyll.Core.Provider
    ( -- * Constructing resource providers
      Internal.Provider
    , newProvider

      -- * Querying resource properties
    , Internal.resourceList
    , Internal.resourceExists
    , Internal.resourceFilePath
    , Internal.resourceModified
    , Internal.resourceModificationTime

      -- * Access to raw resource content
    , Internal.resourceString
    , Internal.resourceLBS

      -- * Access to metadata and body content
    , Internal.resourceMetadata
    , Internal.resourceBody
    ) where

--------------------------------------------------------------------------------
import qualified Hakyll.Core.Provider.Internal      as Internal
import qualified Hakyll.Core.Provider.MetadataCache as Internal
import           Hakyll.Core.Store                  (Store)
import           Hakyll.Core.Identifier             (Identifier)


--------------------------------------------------------------------------------
-- | Create a resource provider
newProvider :: Store                                -- ^ Store to use
            -> (FilePath -> IO Bool)                -- ^ Should we ignore this file?
            -> FilePath                             -- ^ Search directory
            -> IO (Internal.Provider, [Identifier]) -- ^ Resulting provider and modified metadata
newProvider store ignore directory = do
  p <- Internal.newProvider store ignore directory

  let modified =
        filter
          (Internal.resourceModified p)
          (Internal.resourceList p)

  -- Delete metadata cache where necessary and extract identifiers for which the
  -- metadata was actually modified. That is, exclude identifiers where the body
  -- was modified but the metadata wasn't.
  mayModifiedMetadata <- mapM (Internal.resourceInvalidateMetadataCache p) modified
  newMetadata <- mapM (Internal.resourceMetadata p) modified
  let modifiedMetadata =
        map (\(i, _, _) -> i) $
          filter (\(_, nm, om) -> nm /= om) $
            zip3 modified mayModifiedMetadata newMetadata

  return (p, modifiedMetadata)
