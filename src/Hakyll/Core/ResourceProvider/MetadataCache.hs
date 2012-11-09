--------------------------------------------------------------------------------
module Hakyll.Core.ResourceProvider.MetadataCache
    ( resourceMetadata
    , resourceBody
    , resourceInvalidateMetadataCache
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Metadata
import           Hakyll.Core.ResourceProvider.Internal
import           Hakyll.Core.ResourceProvider.Metadata
import qualified Hakyll.Core.Store                     as Store


--------------------------------------------------------------------------------
resourceMetadata :: ResourceProvider -> Identifier a -> IO Metadata
resourceMetadata rp r = do
    load rp r
    Store.Found md <- Store.get (resourceStore rp)
        [name, toFilePath r, "metadata"]
    return md


--------------------------------------------------------------------------------
resourceBody :: ResourceProvider -> Identifier a -> IO String
resourceBody rp r = do
    load rp r
    Store.Found bd <- Store.get (resourceStore rp)
        [name, toFilePath r, "body"]
    maybe (resourceString r) return bd


--------------------------------------------------------------------------------
resourceInvalidateMetadataCache :: ResourceProvider -> Identifier a -> IO ()
resourceInvalidateMetadataCache rp r = do
    Store.delete (resourceStore rp) [name, toFilePath r, "metadata"]
    Store.delete (resourceStore rp) [name, toFilePath r, "body"]


--------------------------------------------------------------------------------
load :: ResourceProvider -> Identifier a -> IO ()
load rp r = do
    mmd <- Store.get store mdk :: IO (Store.Result Metadata)
    case mmd of
        -- Already loaded
        Store.Found _  -> return ()
        -- Not yet loaded
        _ -> do
            (metadata, body) <- loadMetadata rp r
            Store.set store mdk metadata
            Store.set store bk  body
  where
    store = resourceStore rp
    mdk   = [name, toFilePath r, "metadata"]
    bk    = [name, toFilePath r, "body"]


--------------------------------------------------------------------------------
name :: String
name = "Hakyll.Core.Resource.Provider.MetadataCache"
