-- | A store for stroing and retreiving items
--
module Hakyll.Core.Store
    ( Store
    , makeStore
    , storeSet
    , storeGet
    ) where

import Control.Applicative ((<$>))
import System.FilePath ((</>))
import System.Directory (doesFileExist)

import Data.Binary (Binary, encodeFile, decodeFile)

import Hakyll.Core.Identifier
import Hakyll.Core.Util.File

-- | Data structure used for the store
--
data Store = Store
    { storeDirectory :: FilePath
    }

-- | Initialize the store
--
makeStore :: FilePath -> IO Store
makeStore directory = return Store {storeDirectory = directory}

-- | Create a path
--
makePath :: Store -> String -> Identifier -> FilePath
makePath store name identifier =
    storeDirectory store </> name </> toFilePath identifier

-- | Store an item
--
storeSet :: Binary a => Store -> String -> Identifier -> a -> IO ()
storeSet store name identifier value = do
    makeDirectories path
    encodeFile path value
  where
    path = makePath store name identifier

-- | Load an item
--
storeGet :: Binary a => Store -> String -> Identifier -> IO (Maybe a)
storeGet store name identifier = do
    exists <- doesFileExist path
    if exists then Just <$> decodeFile path
              else return Nothing
  where
    path = makePath store name identifier
