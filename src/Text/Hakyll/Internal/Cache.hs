module Text.Hakyll.Internal.Cache
    ( storeInCache
    , getFromCache
    ) where

import Control.Monad.Reader (liftIO)
import Text.Hakyll.Hakyll (Hakyll)
import Text.Hakyll.File
import Data.Binary

storeInCache :: (Binary a) => a -> FilePath -> Hakyll ()
storeInCache value path = do
    cachePath <- toCache path
    makeDirectories cachePath
    liftIO $ encodeFile cachePath value

getFromCache :: (Binary a) => FilePath -> Hakyll (Maybe a)
getFromCache path = do
    cachePath <- toCache path
    valid <- isMoreRecent cachePath [path]
    if valid then liftIO (decodeFile cachePath) >>= return . Just
             else return Nothing
