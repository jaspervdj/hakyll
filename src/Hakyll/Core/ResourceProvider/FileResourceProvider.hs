-- | A concrete 'ResourceProvider' that gets it's resources from the filesystem
--
module Hakyll.Core.ResourceProvider.FileResourceProvider
    ( fileResourceProvider
    ) where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Lazy as LB

import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier
import Hakyll.Core.Util.File
import Hakyll.Core.Configuration

-- | Create a filesystem-based 'ResourceProvider'
--
fileResourceProvider :: HakyllConfiguration -> IO ResourceProvider
fileResourceProvider configuration = do
    -- Retrieve a list of identifiers
    list <- map parseIdentifier . filter (not . ignoreFile configuration) <$>
        getRecursiveContents False "."

    -- Construct a resource provider
    return ResourceProvider
        { resourceList           = map Resource list
        , resourceString         = readFile . toFilePath . unResource
        , resourceLazyByteString = LB.readFile . toFilePath . unResource
        }
