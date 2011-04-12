-- | Dummy resource provider for testing purposes
--
module Hakyll.Core.Resource.Provider.Dummy
    ( dummyResourceProvider
    ) where

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.ByteString.Lazy.Char8 as LBC

import Hakyll.Core.Resource
import Hakyll.Core.Resource.Provider

-- | Create a dummy 'ResourceProvider'
--
dummyResourceProvider :: Map String String -> IO ResourceProvider
dummyResourceProvider vfs = makeResourceProvider
    (map Resource (M.keys vfs))
    (return . (vfs M.!) . unResource)
    (return . LBC.pack . (vfs M.!) . unResource)
