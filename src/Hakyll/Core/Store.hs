-- | A store for stroing and retreiving items
--
module Hakyll.Core.Store
    ( Store
    , makeStore
    , storeSet
    , storeGet
    ) where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Binary (Binary, encodeFile, decodeFile)
import Data.Typeable (Typeable)

import Hakyll.Core.CompiledItem
import Hakyll.Core.Writable
import Hakyll.Core.Identifier
import Hakyll.Core.Util.File

-- | Data structure used for the store
--
data Store = Store
    { -- | All items are stored on the filesystem
      storeDirectory :: FilePath
    , -- | And some items are also kept in-memory
      storeMap :: MVar (Map FilePath CompiledItem)
    }

-- | Initialize the store
--
makeStore :: FilePath -> IO Store
makeStore directory = do
    mvar <- newMVar M.empty
    return Store
        { storeDirectory = directory
        , storeMap       = mvar
        }

-- | Auxiliary: add an item to the map
--
addToMap :: (Binary a, Typeable a, Writable a)
         => Store -> FilePath -> a -> IO ()
addToMap store path value =
    modifyMVar_ (storeMap store) $ return . M.insert path (compiledItem value)

-- | Create a path
--
makePath :: Store -> String -> Identifier -> FilePath
makePath store name identifier =
    storeDirectory store </> name </> toFilePath identifier </> ".hakyllstore"

-- | Store an item
--
storeSet :: (Binary a, Typeable a, Writable a)
         => Store -> String -> Identifier -> a -> IO ()
storeSet store name identifier value = do
    makeDirectories path
    encodeFile path value
    addToMap store path value
  where
    path = makePath store name identifier

-- | Load an item
--
storeGet :: (Binary a, Typeable a, Writable a)
         => Store -> String -> Identifier -> IO (Maybe a)
storeGet store name identifier = do
    -- First check the in-memory map
    map' <- readMVar $ storeMap store
    case M.lookup path map' of
        -- Found in the in-memory map
        Just c -> return $ Just $ unCompiledItem c
        -- Not found in the map, try the filesystem
        Nothing -> do
            exists <- doesFileExist path
            if not exists
                -- Not found in the filesystem either
                then return Nothing
                -- Found in the filesystem
                else do v <- decodeFile path
                        addToMap store path v
                        return $ Just v
  where
    path = makePath store name identifier
