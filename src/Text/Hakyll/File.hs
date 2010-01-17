-- | A module containing various function for manipulating and examinating
--   files and directories.
module Text.Hakyll.File
    ( toDestination
    , toCache
    , toURL
    , toRoot
    , removeSpaces
    , makeDirectories
    , getRecursiveContents
    , havingExtension
    , isCacheValid
    , directory
    ) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List (isPrefixOf)
import Text.Hakyll.Hakyll (Hakyll)
import Control.Monad.Reader (liftIO)

-- | Auxiliary function to remove pathSeparators form the start. We don't deal
--   with absolute paths here. We also remove $root from the start.
removeLeadingSeparator :: FilePath -> FilePath
removeLeadingSeparator [] = []
removeLeadingSeparator path
    | (head path') `elem` pathSeparators = (tail path')
    | otherwise                          = path'
  where
    path' = if "$root" `isPrefixOf` path then drop 5 path
                                         else path

-- | Convert a relative filepath to a filepath in the destination (@_site@).
toDestination :: FilePath -> FilePath
toDestination path = "_site" </> (removeLeadingSeparator path)

-- | Convert a relative filepath to a filepath in the cache (@_cache@).
toCache :: FilePath -> FilePath
toCache path = "_cache" </> (removeLeadingSeparator path)

-- | Get the url for a given page.
toURL :: FilePath -> FilePath
toURL path = if takeExtension path `elem` [".markdown", ".md", ".tex"]
                then flip addExtension ".html" $ dropExtension path
                else path

-- | Get the relative url to the site root, for a given (absolute) url
toRoot :: FilePath -> FilePath
toRoot = emptyException . joinPath . map parent . splitPath
       . takeDirectory . removeLeadingSeparator
  where
    parent = const ".."
    emptyException [] = "."
    emptyException x  = x

-- | Swaps spaces for '-'.
removeSpaces :: FilePath -> FilePath
removeSpaces = map swap
  where
    swap ' ' = '-'
    swap x   = x

-- | Given a path to a file, try to make the path writable by making
--   all directories on the path.
makeDirectories :: FilePath -> Hakyll ()
makeDirectories path = liftIO $ createDirectoryIfMissing True dir
  where
    dir = takeDirectory path

-- | Get all contents of a directory. Note that files starting with a dot (.)
--   will be ignored.
getRecursiveContents :: FilePath -> Hakyll [FilePath]
getRecursiveContents topdir = do
    names <- liftIO $ getDirectoryContents topdir
    let properNames = filter isProper names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- liftIO $ doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)
  where
    isProper = not . (== '.') . head

-- | A filter that takes all file names with a given extension. Prefix the
--   extension with a dot:
--
--   > havingExtension ".markdown" [ "index.markdown"
--   >                             , "style.css"
--   >                             ] == ["index.markdown"]
havingExtension :: String -> [FilePath] -> [FilePath]
havingExtension extension = filter ((==) extension . takeExtension)

-- | Perform a Hakyll action on every file in a given directory.
directory :: (FilePath -> Hakyll ()) -> FilePath -> Hakyll ()
directory action dir = do
    contents <- getRecursiveContents dir
    mapM_ action contents

-- | Check if a cache file is still valid.
isCacheValid :: FilePath -- ^ The cached file.
             -> [FilePath] -- ^ Dependencies of the cached file.
             -> Hakyll Bool
isCacheValid cache depends = do
    exists <- liftIO $ doesFileExist cache
    if not exists
        then return False
        else do dependsModified <- liftIO $ mapM getModificationTime depends
                cacheModified <- liftIO $ getModificationTime cache
                return (cacheModified >= maximum dependsModified)
