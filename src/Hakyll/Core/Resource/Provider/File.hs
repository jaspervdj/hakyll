-- | A concrete 'ResourceProvider' that gets it's resources from the filesystem
--
module Hakyll.Core.Resource.Provider.File
    ( fileResourceProvider
    ) where

import Control.Applicative ((<$>))

import Data.Time (readTime)
import System.Directory (getModificationTime)
import System.Locale (defaultTimeLocale)
import System.Time (formatCalendarTime, toCalendarTime)
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
    list <- map resource . filter (not . shouldIgnoreFile configuration) <$>
        getRecursiveContents False "."
    makeResourceProvider list (readFile . unResource)
                              (LB.readFile . unResource)
                              (mtime . unResource)
  where
    mtime r = do
        ct <- toCalendarTime =<< getModificationTime r
        let str = formatCalendarTime defaultTimeLocale "%s" ct
        return $ readTime defaultTimeLocale "%s" str
