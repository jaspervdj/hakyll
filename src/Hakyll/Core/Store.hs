-- | A store for stroing and retreiving items
--
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Hakyll.Core.Store
    ( Store
    , StoreGet (..)
    , makeStore
    , storeSet
    , storeGet
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Binary (Binary, encodeFile, decodeFile)
import Data.Typeable (Typeable, TypeRep, cast, typeOf)

import Hakyll.Core.Identifier
import Hakyll.Core.Util.File

-- | Items we can store
--
data Storable = forall a. (Binary a, Typeable a) => Storable a

-- | Result when an item from the store
--
data StoreGet a = Found a
                | NotFound
                | WrongType TypeRep TypeRep
                deriving (Show, Eq)

-- | Data structure used for the store
--
data Store = Store
    { -- | All items are stored on the filesystem
      storeDirectory :: FilePath
    , -- | And some items are also kept in-memory
      storeMap       :: Maybe (MVar (Map FilePath Storable))
    }

-- | Initialize the store
--
makeStore :: Bool      -- ^ Use in-memory caching
          -> FilePath  -- ^ Directory to use for hard disk storage
          -> IO Store  -- ^ Store
makeStore inMemory directory = do
    mvar <- if inMemory then Just <$> newMVar M.empty else return Nothing
    return Store
        { storeDirectory = directory
        , storeMap       = mvar
        }

-- | Auxiliary: add an item to the map
--
cacheInsert :: (Binary a, Typeable a) => Store -> FilePath -> a -> IO ()
cacheInsert (Store _ Nothing)   _    _     = return ()
cacheInsert (Store _ (Just mv)) path value =
    modifyMVar_ mv $ return . M.insert path (Storable value)

-- | Auxiliary: get an item from the cache
--
cacheLookup :: forall a. (Binary a, Typeable a)
            => Store -> FilePath -> IO (StoreGet a)
cacheLookup (Store _ Nothing) _      = return NotFound
cacheLookup (Store _ (Just mv)) path = do
    map' <- readMVar mv
    case M.lookup path map' of
        Nothing           -> return NotFound
        Just (Storable s) -> return $ case cast s of
            Nothing -> WrongType (typeOf s) $ typeOf (undefined :: a)
            Just s' -> Found s'

-- | Create a path
--
makePath :: Store -> String -> Identifier a -> FilePath
makePath store name identifier = storeDirectory store </> name
    </> group </> toFilePath identifier </> "hakyllstore"
  where
    group = fromMaybe "" $ identifierGroup identifier

-- | Store an item
--
storeSet :: (Binary a, Typeable a)
         => Store -> String -> Identifier a -> a -> IO ()
storeSet store name identifier value = do
    makeDirectories path
    encodeFile path value
    cacheInsert store path value
  where
    path = makePath store name identifier

-- | Load an item
--
storeGet :: (Binary a, Typeable a)
         => Store -> String -> Identifier a -> IO (StoreGet a)
storeGet store name identifier = do
    -- First check the in-memory map
    mv <- cacheLookup store path
    case mv of
        -- Not found in the map, try the filesystem
        NotFound -> do
            exists <- doesFileExist path
            if not exists
                -- Not found in the filesystem either
                then return NotFound
                -- Found in the filesystem
                else do v <- decodeFile path
                        cacheInsert store path v
                        return $ Found v
        -- Found in the in-memory map, just return
        s -> return s
  where
    path = makePath store name identifier
