--------------------------------------------------------------------------------
module Hakyll.Core.Resource.Modified
    ( resourceModified
    , resourceModificationTime
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                    ((<$>), (<*>))
import           Control.Monad                          (when)
import qualified Crypto.Hash.MD5                        as MD5
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Lazy                   as BL
import           Data.IORef
import qualified Data.Map                               as M
import           Data.Time                              (UTCTime)
import           System.Directory                       (getModificationTime)


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.MetadataCache
import           Hakyll.Core.Resource.Provider.Internal
import           Hakyll.Core.Store                      (Store)
import qualified Hakyll.Core.Store                      as Store


--------------------------------------------------------------------------------
-- | A resource is modified if it or its metadata has changed
resourceModified :: ResourceProvider -> Resource -> IO Bool
resourceModified rp r
    | not exists = return False
    | otherwise  = do
        cache <- readIORef cacheRef
        case M.lookup r cache of
            Just m  -> return m
            Nothing -> do
                -- Check if the actual file was modified, and do a recursive
                -- call to check if the metadata file was modified
                m <- (||)
                    <$> fileDigestModified store (unResource r)
                    <*> resourceModified rp (resourceMetadataResource r)
                modifyIORef cacheRef (M.insert r m)

                -- Important! (But ugly)
                when m $ resourceInvalidateMetadataCache rp r

                return m
  where
    exists   = resourceExists rp r
    store    = resourceStore rp
    cacheRef = resourceModifiedCache rp


--------------------------------------------------------------------------------
-- | Utility: Check if a the digest of a file was modified
fileDigestModified :: Store -> FilePath -> IO Bool
fileDigestModified store fp = do
    -- Get the latest seen digest from the store, and calculate the current
    -- digest for the
    lastDigest <- Store.get store key
    newDigest  <- fileDigest fp
    if Store.Found newDigest == lastDigest
        -- All is fine, not modified
        then return False
        -- Resource modified; store new digest
        else do
            Store.set store key newDigest
            return True
  where
    key = ["Hakyll.Core.Resource.Provider.fileModified", fp]


--------------------------------------------------------------------------------
-- | Utility: Retrieve a digest for a given file
fileDigest :: FilePath -> IO B.ByteString
fileDigest = fmap MD5.hashlazy . BL.readFile


--------------------------------------------------------------------------------
resourceModificationTime :: Resource -> IO UTCTime
resourceModificationTime = getModificationTime . unResource
