--------------------------------------------------------------------------------
-- | A store for storing and retreiving items
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Hakyll.Core.Store
    ( Store
    , Result (..)
    , toMaybe
    , new
    , set
    , get
    , delete
    , hash
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Exception   (IOException, handle)
import qualified Crypto.Hash.MD5     as MD5
import           Data.Binary         (Binary, decodeFile, encodeFile)
import qualified Data.ByteString     as B
import qualified Data.Cache.LRU.IO   as Lru
import           Data.List           (intercalate)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Data.Typeable       (TypeRep, Typeable, cast, typeOf)
import           System.Directory    (createDirectoryIfMissing)
import           System.Directory    (doesFileExist, removeFile)
import           System.FilePath     ((</>))
import           Text.Printf         (printf)


--------------------------------------------------------------------------------
-- | Simple wrapper type
data Box = forall a. Typeable a => Box a


--------------------------------------------------------------------------------
data Store = Store
    { -- | All items are stored on the filesystem
      storeDirectory :: FilePath
    , -- | Optionally, items are also kept in-memory
      storeMap       :: Maybe (Lru.AtomicLRU FilePath Box)
    }


--------------------------------------------------------------------------------
-- | Result of a store query
data Result a
    = Found a                    -- ^ Found, result
    | NotFound                   -- ^ Not found
    | WrongType TypeRep TypeRep  -- ^ Expected, true type
    deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | Convert result to 'Maybe'
toMaybe :: Result a -> Maybe a
toMaybe (Found x) = Just x
toMaybe _         = Nothing


--------------------------------------------------------------------------------
-- | Initialize the store
new :: Bool      -- ^ Use in-memory caching
    -> FilePath  -- ^ Directory to use for hard disk storage
    -> IO Store  -- ^ Store
new inMemory directory = do
    createDirectoryIfMissing True directory
    ref <- if inMemory then Just <$> Lru.newAtomicLRU csize else return Nothing
    return Store
        { storeDirectory = directory
        , storeMap       = ref
        }
  where
    csize = Just 500


--------------------------------------------------------------------------------
-- | Auxiliary: add an item to the in-memory cache
cacheInsert :: Typeable a => Store -> String -> a -> IO ()
cacheInsert (Store _ Nothing)    _   _     = return ()
cacheInsert (Store _ (Just lru)) key x =
    Lru.insert key (Box x) lru


--------------------------------------------------------------------------------
-- | Auxiliary: get an item from the in-memory cache
cacheLookup :: forall a. Typeable a => Store -> String -> IO (Result a)
cacheLookup (Store _ Nothing)    _   = return NotFound
cacheLookup (Store _ (Just lru)) key = do
    res <- Lru.lookup key lru
    return $ case res of
        Nothing      -> NotFound
        Just (Box x) -> case cast x of
            Just x' -> Found x'
            Nothing -> WrongType (typeOf (undefined :: a)) (typeOf x)


--------------------------------------------------------------------------------
-- | Auxiliary: delete an item from the in-memory cache
cacheDelete :: Store -> String -> IO ()
cacheDelete (Store _ Nothing)    _   = return ()
cacheDelete (Store _ (Just lru)) key = do
    _ <- Lru.delete key lru
    return ()


--------------------------------------------------------------------------------
-- | Store an item
set :: (Binary a, Typeable a) => Store -> [String] -> a -> IO ()
set store identifier value = do
    encodeFile (storeDirectory store </> key) value
    cacheInsert store key value
  where
    key = hash identifier


--------------------------------------------------------------------------------
-- | Load an item
get :: (Binary a, Typeable a) => Store -> [String] -> IO (Result a)
get store identifier = do
    -- First check the in-memory map
    ref <- cacheLookup store key
    case ref of
        -- Not found in the map, try the filesystem
        NotFound -> do
            exists <- doesFileExist path
            if not exists
                -- Not found in the filesystem either
                then return NotFound
                -- Found in the filesystem
                else do
                    v <- decodeFile path
                    cacheInsert store key v
                    return $ Found v
        -- Found in the in-memory map (or wrong type), just return
        s -> return s
  where
    key  = hash identifier
    path = storeDirectory store </> key


--------------------------------------------------------------------------------
-- | Delete an item
delete :: Store -> [String] -> IO ()
delete store identifier = do
    cacheDelete store key
    deleteFile $ storeDirectory store </> key
  where
    key  = hash identifier


--------------------------------------------------------------------------------
-- | Delete a file unless it doesn't exist...
deleteFile :: FilePath -> IO ()
deleteFile = handle (\(_ :: IOException) -> return ()) . removeFile


--------------------------------------------------------------------------------
-- | Mostly meant for internal usage
hash :: [String] -> String
hash = concatMap (printf "%02x") . B.unpack .
    MD5.hash . T.encodeUtf8 . T.pack . intercalate "/"
