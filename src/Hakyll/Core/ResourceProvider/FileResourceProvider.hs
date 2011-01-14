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

-- | Create a filesystem-based 'ResourceProvider'
--
fileResourceProvider :: IO ResourceProvider
fileResourceProvider = do
    list <- map parseIdentifier <$> getRecursiveContents "."
    return ResourceProvider
        { resourceList           = list
        , resourceString         = readFile . toFilePath
        , resourceLazyByteString = LB.readFile . toFilePath
        }
