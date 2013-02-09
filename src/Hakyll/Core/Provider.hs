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
import           Control.Monad                      (forM_)
import qualified Hakyll.Core.Provider.Internal      as Internal
import qualified Hakyll.Core.Provider.MetadataCache as Internal
import           Hakyll.Core.Store                  (Store)


--------------------------------------------------------------------------------
-- | Create a resource provider
newProvider :: Store                 -- ^ Store to use
            -> (FilePath -> Bool)    -- ^ Should we ignore this file?
            -> FilePath              -- ^ Search directory
            -> IO Internal.Provider  -- ^ Resulting provider
newProvider store ignore directory = do
    -- Delete metadata cache where necessary
    provider <- Internal.newProvider store ignore directory
    forM_ (Internal.resourceList provider) $ \identifier ->
        if Internal.resourceModified provider identifier
            then Internal.resourceInvalidateMetadataCache provider identifier
            else return ()
    return provider
