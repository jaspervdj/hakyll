-- | Dummy resource provider for testing purposes
--
module Hakyll.Core.Resource.Provider.Dummy
    ( dummyResourceProvider
    ) where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Time (getCurrentTime)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Hakyll.Core.Resource
import Hakyll.Core.Resource.Provider

-- | Create a dummy 'ResourceProvider'
--
dummyResourceProvider :: Map String ByteString -> IO ResourceProvider
dummyResourceProvider vfs = makeResourceProvider
    (map resource (M.keys vfs))
    (return . TL.unpack . TL.decodeUtf8 . (vfs M.!) . unResource)
    (return . (vfs M.!) . unResource)
    (const getCurrentTime)
