--------------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Provider.Internal
    ( ResourceInfo (..)
    , Provider (..)
    , newProvider

    , resourceList
    , resourceExists

    , resourceFilePath
    , resourceString
    , resourceLBS

    , resourceModified
    , resourceModificationTime
    ) where


--------------------------------------------------------------------------------
import           Control.DeepSeq        (NFData (..), deepseq)
import           Control.Monad          (forM)
import           Control.Applicative    ((<|>))
import           Data.Binary            (Binary (..))
import qualified Data.ByteString.Lazy   as BL
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Data.Time              (Day (..), UTCTime (..))
import           Data.Typeable          (Typeable)
import           System.Directory       (getModificationTime)
import           System.FilePath        (stripExtension, (</>))


--------------------------------------------------------------------------------
#if !MIN_VERSION_directory(1,2,0)
import           Data.Time              (readTime)
import           System.Locale          (defaultTimeLocale)
import           System.Time            (formatCalendarTime, toCalendarTime)
#endif


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Store      (Store)
import qualified Hakyll.Core.Store      as Store
import           Hakyll.Core.Util.File


--------------------------------------------------------------------------------
-- | Because UTCTime doesn't have a Binary instance...
newtype BinaryTime = BinaryTime {unBinaryTime :: UTCTime}
    deriving (Eq, NFData, Ord, Show, Typeable)


--------------------------------------------------------------------------------
instance Binary BinaryTime where
    put (BinaryTime (UTCTime (ModifiedJulianDay d) dt)) =
        put d >> put (toRational dt)

    get = fmap BinaryTime $ UTCTime
        <$> (ModifiedJulianDay <$> get)
        <*> (fromRational <$> get)


--------------------------------------------------------------------------------
data ResourceInfo = ResourceInfo
    { resourceInfoModified :: BinaryTime
    , resourceInfoMetadata :: Maybe Identifier
    } deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Binary ResourceInfo where
    put (ResourceInfo mtime meta) = put mtime >> put meta
    get = ResourceInfo <$> get <*> get


--------------------------------------------------------------------------------
instance NFData ResourceInfo where
    rnf (ResourceInfo mtime meta) = rnf mtime `seq` rnf meta `seq` ()


--------------------------------------------------------------------------------
-- | Responsible for retrieving and listing resources
data Provider = Provider
    { -- Top of the provided directory
      providerDirectory :: FilePath
    , -- | A list of all files found
      providerFiles     :: Map Identifier ResourceInfo
    , -- | A list of the files from the previous run
      providerOldFiles  :: Map Identifier ResourceInfo
    , -- | Underlying persistent store for caching
      providerStore     :: Store
    } deriving (Show)


--------------------------------------------------------------------------------
-- | Create a resource provider
newProvider :: Store                  -- ^ Store to use
            -> (FilePath -> IO Bool)  -- ^ Should we ignore this file?
            -> FilePath               -- ^ Search directory
            -> IO Provider            -- ^ Resulting provider
newProvider store ignore directory = do
    list <- map fromFilePath <$> getRecursiveContents ignore directory
    files <- M.fromListWith combine <$> mapM (getResourceInfo directory) list

    -- Get the old files from the store, and then immediately replace them by
    -- the new files.
    oldFiles <- fromMaybe mempty . Store.toMaybe <$> Store.get store oldKey
    oldFiles `deepseq` Store.set store oldKey files

    return $ Provider directory files oldFiles store
  where
    oldKey = ["Hakyll.Core.Provider.Internal.newProvider", "oldFiles"]

    -- Combine a resource with its metadata file
    combine (ResourceInfo xTime xMeta) (ResourceInfo yTime yMeta) =
        ResourceInfo (xTime `max` yTime) (xMeta <|> yMeta)


--------------------------------------------------------------------------------
getResourceInfo :: FilePath -> Identifier -> IO (Identifier, ResourceInfo)
getResourceInfo directory identifier = do
    let file = toFilePath identifier
    mtime <- fileModificationTime $ directory </> file
    let makeInfo m = ResourceInfo (BinaryTime mtime) m
    return $ case stripExtension "metadata" file of
        Nothing -> (identifier, makeInfo Nothing)
        Just r -> (fromFilePath r, makeInfo (Just identifier))


--------------------------------------------------------------------------------
resourceList :: Provider -> [Identifier]
resourceList = M.keys . providerFiles


--------------------------------------------------------------------------------
-- | Check if a given resource exists
resourceExists :: Provider -> Identifier -> Bool
resourceExists provider =
    (`M.member` providerFiles provider) . setVersion Nothing


--------------------------------------------------------------------------------
resourceFilePath :: Provider -> Identifier -> FilePath
resourceFilePath p i = providerDirectory p </> toFilePath i


--------------------------------------------------------------------------------
-- | Get the raw body of a resource as string
resourceString :: Provider -> Identifier -> IO String
resourceString p i = readFile $ resourceFilePath p i


--------------------------------------------------------------------------------
-- | Get the raw body of a resource of a lazy bytestring
resourceLBS :: Provider -> Identifier -> IO BL.ByteString
resourceLBS p i = BL.readFile $ resourceFilePath p i


--------------------------------------------------------------------------------
-- | A resource is modified if it or its metadata has changed
resourceModified :: Provider -> Identifier -> Bool
resourceModified p r = case (ri, oldRi) of
    (Nothing, _)      -> False
    (Just _, Nothing) -> True
    (Just n, Just o)  ->
        resourceInfoModified n >  resourceInfoModified o ||
        resourceInfoMetadata n /= resourceInfoMetadata o
  where
    normal = setVersion Nothing r
    ri     = M.lookup normal (providerFiles p)
    oldRi  = M.lookup normal (providerOldFiles p)


--------------------------------------------------------------------------------
resourceModificationTime :: Provider -> Identifier -> UTCTime
resourceModificationTime p i =
    case M.lookup (setVersion Nothing i) (providerFiles p) of
        Just ri -> unBinaryTime $ resourceInfoModified ri
        Nothing -> error $
            "Hakyll.Core.Provider.Internal.resourceModificationTime: " ++
            "resource " ++ show i ++ " does not exist"


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
