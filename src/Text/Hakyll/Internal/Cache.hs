module Text.Hakyll.Internal.Cache
    ( storeInCache
    , getFromCache
    ) where

import Control.Monad.Reader (liftIO)
import Text.Hakyll.Hakyll (Hakyll)
import Text.Hakyll.File
import Data.Binary

-- | We can store all datatypes instantiating @Binary@ to the cache. The cache
--   directory is specified by the @HakyllConfiguration@, usually @_cache@.
storeInCache :: (Binary a) => a -> FilePath -> Hakyll ()
storeInCache value path = do
    cachePath <- toCache path
    makeDirectories cachePath
    liftIO $ encodeFile cachePath value

-- | Get a value from the cache. The filepath given should not be located in the
--   cache. This function performs a timestamp check on the filepath and the
--   filepath in the cache, and only returns the cached value when it is still
--   up-to-date.
getFromCache :: (Binary a) => FilePath -> Hakyll (Maybe a)
getFromCache path = do
    cachePath <- toCache path
    valid <- isMoreRecent cachePath [path]
    if valid then liftIO (decodeFile cachePath) >>= return . Just
             else return Nothing
