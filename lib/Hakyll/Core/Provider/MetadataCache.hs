--------------------------------------------------------------------------------
module Hakyll.Core.Provider.MetadataCache
    ( resourceMetadata
    , resourceBody
    , resourceLookupMetadataCache
    , resourceInvalidateMetadataCache
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                 (unless)
import           Data.Maybe                    (fromMaybe)
import           Hakyll.Core.Identifier
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Internal
import           Hakyll.Core.Provider.Metadata
import qualified Hakyll.Core.Store             as Store


--------------------------------------------------------------------------------
resourceMetadata :: Provider -> Identifier -> IO Metadata
resourceMetadata p r
    | not (resourceExists p r) = return mempty
    | otherwise                = do
        -- TODO keep time in md cache
        load p r
        fromMaybe mempty <$> resourceLookupMetadataCache p r


--------------------------------------------------------------------------------
resourceBody :: Provider -> Identifier -> IO String
resourceBody p r = do
    load p r
    Store.Found bd <- Store.get (providerStore p)
        [name, toFilePath r, "body"]
    maybe (resourceString p r) return bd


--------------------------------------------------------------------------------
-- | Perform a lookup for metadata in the cache only.
-- Useful internally for invalidation.
resourceLookupMetadataCache :: Provider -> Identifier -> IO (Maybe Metadata)
resourceLookupMetadataCache p r = do
    result <- Store.get (providerStore p) [name, toFilePath r, "metadata"]
    pure $ case result of
      Store.Found (BinaryMetadata m) -> Just m
      Store.NotFound                 -> Nothing
      Store.WrongType _ _            -> error "unexpected WrongType"


--------------------------------------------------------------------------------
resourceInvalidateMetadataCache :: Provider -> Identifier -> IO ()
resourceInvalidateMetadataCache p r = do
    Store.delete (providerStore p) [name, toFilePath r, "metadata"]
    Store.delete (providerStore p) [name, toFilePath r, "body"]


--------------------------------------------------------------------------------
load :: Provider -> Identifier -> IO ()
load p r = do
    mmof <- Store.isMember store mdk
    unless mmof $ do
        (md, body) <- loadMetadata p r
        Store.set store mdk (BinaryMetadata md)
        Store.set store bk  body
  where
    store = providerStore p
    mdk   = [name, toFilePath r, "metadata"]
    bk    = [name, toFilePath r, "body"]


--------------------------------------------------------------------------------
name :: String
name = "Hakyll.Core.Resource.Provider.MetadataCache"
