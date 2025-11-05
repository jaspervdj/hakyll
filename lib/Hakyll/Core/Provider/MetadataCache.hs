--------------------------------------------------------------------------------
module Hakyll.Core.Provider.MetadataCache
    ( resourceMetadata
    , resourceBody
    , resourceInvalidateMetadataCache
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                 (unless)
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
        Store.Found (BinaryMetadata md) <- Store.get (providerStore p)
            [name, toFilePath r, "metadata"]
        return md


--------------------------------------------------------------------------------
resourceBody :: Provider -> Identifier -> IO String
resourceBody p r = do
    load p r
    Store.Found bd <- Store.get (providerStore p)
        [name, toFilePath r, "body"]
    maybe (resourceString p r) return bd


--------------------------------------------------------------------------------
resourceInvalidateMetadataCache :: Provider -> Identifier -> IO (Metadata)
resourceInvalidateMetadataCache p r = do
    result <- Store.get (providerStore p)
        [name, toFilePath r, "metadata"]
    let md = case result of
          Store.Found (BinaryMetadata m) -> m
          Store.NotFound -> mempty
          Store.WrongType _ _ -> error "unexpected WrongType"

    Store.delete (providerStore p) [name, toFilePath r, "metadata"]
    Store.delete (providerStore p) [name, toFilePath r, "body"]

    pure md


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
