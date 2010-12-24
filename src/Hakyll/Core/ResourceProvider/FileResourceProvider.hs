-- | A concrete 'ResourceProvider' that gets it's resources from the filesystem
--
module Hakyll.Core.ResourceProvider.FileResourceProvider
    ( fileResourceProvider
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), normalise)

import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier

-- | Create a filesystem-based 'ResourceProvider'
--
fileResourceProvider :: IO ResourceProvider
fileResourceProvider = do
    list <- map parseIdentifier <$> getRecursiveContents "."
    return $ ResourceProvider
        { resourceList   = list
        , resourceString = readFile . toFilePath
        }

-- | Get all contents of a directory. Note that files starting with a dot (.)
--   will be ignored.
--
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    topdirExists <- doesDirectoryExist topdir
    if topdirExists
        then do names <- getDirectoryContents topdir
                let properNames = filter isProper names
                paths <- forM properNames $ \name -> do
                    let path = topdir </> name
                    isDirectory <- doesDirectoryExist path
                    if isDirectory
                        then getRecursiveContents path
                        else return [normalise path]
                return (concat paths)
        else return []
  where
    isProper = not . (== '.') . head
