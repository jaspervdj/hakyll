--------------------------------------------------------------------------------
module Hakyll.Core.Resource.MetadataCache
    ( resourceMetadata
    , resourceBody
    , resourceInvalidateMetadataCache
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Metadata
import           Hakyll.Core.Resource.Provider.Internal
import qualified Hakyll.Core.Store                      as Store


--------------------------------------------------------------------------------
resourceMetadata :: ResourceProvider -> Resource -> IO Metadata
resourceMetadata rp r = do
    load rp r
    Store.Found md <- Store.get (resourceStore rp)
        [name, unResource r, "metadata"]
    return md


--------------------------------------------------------------------------------
resourceBody :: ResourceProvider -> Resource -> IO String
resourceBody rp r = do
    load rp r
    Store.Found bd <- Store.get (resourceStore rp)
        [name, unResource r, "body"]
    maybe (resourceString r) return bd


--------------------------------------------------------------------------------
resourceInvalidateMetadataCache :: ResourceProvider -> Resource -> IO ()
resourceInvalidateMetadataCache rp r = do
    Store.delete (resourceStore rp) [name, unResource r, "metadata"]
    Store.delete (resourceStore rp) [name, unResource r, "body"]


--------------------------------------------------------------------------------
load :: ResourceProvider -> Resource -> IO ()
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
    mdk   = [name, unResource r, "metadata"]
    bk    = [name, unResource r, "body"]


--------------------------------------------------------------------------------
name :: String
name = "Hakyll.Core.Resource.Provider.MetadataCache"
