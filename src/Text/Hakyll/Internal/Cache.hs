module Text.Hakyll.Internal.Cache
    ( storeInCache
    , getFromCache
    ) where

import Control.Monad.Reader (liftIO)
import Text.Hakyll.Hakyll (Hakyll)
import Text.Hakyll.File

storeInCache :: (Show a) => a -> FilePath -> Hakyll ()
storeInCache value path = do
    cachePath <- toCache path
    makeDirectories cachePath
    liftIO $ writeFile cachePath (show value)

getFromCache :: (Read a) => FilePath -> Hakyll (Maybe a)
getFromCache path = do
    cachePath <- toCache path
    valid <- isMoreRecent cachePath [path]
    if valid then liftIO (getFromCache' cachePath) >>= return . Just
             else return Nothing
  where
    getFromCache' cachePath = do c <- readFile cachePath
                                 return (read c)
