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
    , isMember
    , delete
    , hash
    ) where


--------------------------------------------------------------------------------
import qualified Data.Hashable        as DH
import           Data.Binary          (Binary, decode, encodeFile)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Cache.LRU.IO    as Lru
import           Data.List            (intercalate)
import           Data.Maybe           (isJust)
import           Data.Typeable        (TypeRep, Typeable, cast, typeOf)
import           System.Directory     (createDirectoryIfMissing, doesFileExist, removeFile)
import           System.FilePath      ((</>))
import           System.IO            (IOMode (..), hClose, openFile)
import           System.IO.Error      (catchIOError, ioeSetFileName,
                                       ioeSetLocation, modifyIOError)


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
instance Show Store where
    show _ = "<Store>"


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
withStore :: Store -> String -> (String -> FilePath -> IO a) -> [String] -> IO a
withStore store loc run identifier = modifyIOError handle $ run key path
  where
    key = hash identifier
    path = storeDirectory store </> key
    handle e = e `ioeSetFileName` (path ++ " for " ++ intercalate "/" identifier)
                 `ioeSetLocation` ("Store." ++ loc)

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
cacheIsMember :: Store -> String -> IO Bool
cacheIsMember (Store _ Nothing)    _   = return False
cacheIsMember (Store _ (Just lru)) key = isJust <$> Lru.lookup key lru


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
set store identifier value = withStore store "set" (\key path -> do
    encodeFile path value
    cacheInsert store key value
  ) identifier


--------------------------------------------------------------------------------
-- | Load an item
get :: (Binary a, Typeable a) => Store -> [String] -> IO (Result a)
get store = withStore store "get" $ \key path -> do
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
                    v <- decodeClose path
                    cacheInsert store key v
                    return $ Found v
        -- Found in the in-memory map (or wrong type), just return
        s -> return s
  where
    -- 'decodeFile' from Data.Binary which closes the file ASAP
    decodeClose path = do
        h   <- openFile path ReadMode
        lbs <- BL.hGetContents h
        BL.length lbs `seq` hClose h
        return $ decode lbs


--------------------------------------------------------------------------------
-- | Strict function
isMember :: Store -> [String] -> IO Bool
isMember store = withStore store "isMember" $ \key path -> do
    inCache <- cacheIsMember store key
    if inCache then return True else doesFileExist path


--------------------------------------------------------------------------------
-- | Delete an item
delete :: Store -> [String] -> IO ()
delete store = withStore store "delete" $ \key path -> do
    cacheDelete store key
    deleteFile path


--------------------------------------------------------------------------------
-- | Delete a file unless it doesn't exist...
deleteFile :: FilePath -> IO ()
deleteFile = (`catchIOError` \_ -> return ()) . removeFile


--------------------------------------------------------------------------------
-- | Mostly meant for internal usage
hash :: [String] -> String
hash = show . DH.hash . intercalate "/"