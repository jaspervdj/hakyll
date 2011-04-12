-- | A concrete 'ResourceProvider' that gets it's resources from the filesystem
--
module Hakyll.Core.Resource.Provider.File
    ( fileResourceProvider
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (newMVar)
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as LB

import Hakyll.Core.Resource
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Util.File
import Hakyll.Core.Configuration

-- | Create a filesystem-based 'ResourceProvider'
--
fileResourceProvider :: HakyllConfiguration -> IO ResourceProvider
fileResourceProvider configuration = do
    -- Retrieve a list of paths
    list <- filter (not . ignoreFile configuration) <$>
        getRecursiveContents False "."

    -- MVar for the cache
    mvar <- newMVar M.empty

    -- Construct a resource provider
    return ResourceProvider
        { resourceList           = map Resource list
        , resourceString         = readFile . unResource
        , resourceLazyByteString = LB.readFile . unResource
        , resourceModifiedCache  = mvar
        }
