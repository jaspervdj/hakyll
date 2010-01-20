module Text.Hakyll.Internal.Cache
    ( storeInCache
    , getFromCache
    ) where

import Text.Hakyll.Hakyll (Hakyll)

storeInCache :: (Show a) => a -> FilePath -> Hakyll ()
storeInCache = undefined

getFromCache :: (Read a) => FilePath -> Hakyll (Maybe a)
getFromCache = undefined
