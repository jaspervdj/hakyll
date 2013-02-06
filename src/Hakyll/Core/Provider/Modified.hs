--------------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Core.Provider.Modified
    ( resourceModified
    , resourceModificationTime
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                      (when)
import           Data.Binary                        (Binary (..))
import           Data.IORef
import qualified Data.Map                           as M
import           Data.Time                          (Day (..), UTCTime (..),
                                                     secondsToDiffTime)
import           Data.Typeable                      (Typeable)
import           System.Directory                   (getModificationTime)


--------------------------------------------------------------------------------
#if !MIN_VERSION_directory(1,2,0)
import           Data.Time                          (readTime)
import           System.Locale                      (defaultTimeLocale)
import           System.Time                        (formatCalendarTime,
                                                     toCalendarTime)
#endif


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Provider.Internal
import           Hakyll.Core.Provider.MetadataCache
import           Hakyll.Core.Store                  (Store)
import qualified Hakyll.Core.Store                  as Store


--------------------------------------------------------------------------------
-- | A resource is modified if it or its metadata has changed
resourceModified :: Provider -> Identifier -> IO Bool
resourceModified p r
    | not exists = return False
    | otherwise  = do
        cache <- readIORef cacheRef
        case M.lookup normalized cache of
            Just m  -> return m
            Nothing -> do
                -- Check if the actual file was modified, and do a recursive
                -- call to check if the metadata file was modified
                m <- (||)
                    <$> fileModified store filePath
                    <*> resourceModified p (resourceMetadataResource r)
                modifyIORef cacheRef (M.insert normalized m)

                -- Important! (But ugly)
                when m $ resourceInvalidateMetadataCache p r

                return m
  where
    normalized = setVersion Nothing r
    exists     = resourceExists p r
    store      = providerStore p
    cacheRef   = providerModifiedCache p
    filePath   = resourceFilePath p r


--------------------------------------------------------------------------------
-- | Utility: Check if a file was modified recently
fileModified :: Store -> FilePath -> IO Bool
fileModified store fp = do
    lastModified <- Store.get store key
    newModified  <- BinaryTime <$> fileModificationTime fp
    if maybe False (>= newModified) (Store.toMaybe lastModified)
        -- All is fine, not modified
        then return False
        -- Resource modified; store new digest
        else do
            Store.set store key newModified
            return True
  where
    key = ["Hakyll.Core.Resource.Provider.fileModified", fp]


--------------------------------------------------------------------------------
resourceModificationTime :: Provider -> Identifier -> IO UTCTime
resourceModificationTime p i = fileModificationTime $ resourceFilePath p i


--------------------------------------------------------------------------------
fileModificationTime :: FilePath -> IO UTCTime
fileModificationTime fp = do
#if MIN_VERSION_directory(1,2,0)
    getModificationTime fp
#else
    ct <- toCalendarTime =<< getModificationTime fp
    let str = formatCalendarTime defaultTimeLocale "%s" ct
    return $ readTime defaultTimeLocale "%s" str
#endif


--------------------------------------------------------------------------------
-- | Because UTCTime doesn't have a Binary instance...
newtype BinaryTime = BinaryTime UTCTime
    deriving (Eq, Ord, Typeable)


--------------------------------------------------------------------------------
instance Binary BinaryTime where
    put (BinaryTime (UTCTime (ModifiedJulianDay d) dt)) =
        put d >> put (floor dt :: Integer)

    get = fmap BinaryTime $ UTCTime
        <$> (ModifiedJulianDay <$> get)
        <*> (secondsToDiffTime <$> get)
