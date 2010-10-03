module Text.Hakyll.Internal.Cache
    ( storeInCache
    , getFromCache
    , isCacheMoreRecent
    , cacheAction
    ) where

import Control.Monad ((<=<))
import Control.Monad.Reader (liftIO)
import Data.Binary
import System.FilePath ((</>))

import Text.Hakyll.File
import Text.Hakyll.HakyllMonad (Hakyll)
import Text.Hakyll.HakyllAction

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
getFromCache :: (Binary a) => FilePath -> Hakyll a
getFromCache = liftIO . decodeFile <=< toCache

-- | Check if a file in the cache is more recent than a number of other files.
isCacheMoreRecent :: FilePath -> [FilePath] -> Hakyll Bool
isCacheMoreRecent file depends = toCache file >>= flip isFileMoreRecent depends

-- | Cache an entire arrow
--
cacheAction :: Binary a
            => String
            -> HakyllAction () a
            -> HakyllAction () a
cacheAction key action = action { actionFunction = const cacheFunction }
  where
    cacheFunction = do
        -- Construct a filename
        fileName <- fmap (key </>) $ either id (const $ return "unknown")
                                   $ actionUrl action
        -- Check the cache
        cacheOk <- isCacheMoreRecent fileName $ actionDependencies action
        if cacheOk then getFromCache fileName
                   else do result <- actionFunction action ()
                           storeInCache result fileName
                           return result
