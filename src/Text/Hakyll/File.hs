-- | A module containing various function for manipulating and examinating
--   files and directories.
module Text.Hakyll.File
    ( toDestination
    , toCache
    , toURL
    , makeDirectories
    , getRecursiveContents
    , havingExtension
    , isCacheValid
    , directory
    ) where

import System.Directory
import System.FilePath
import Control.Monad

-- | Convert a relative filepath to a filepath in the destination (_site).
toDestination :: FilePath -> FilePath
toDestination path = "_site" </> path

-- | Convert a relative filepath to a filepath in the cache (_cache).
toCache :: FilePath -> FilePath
toCache path = "_cache" </> path

-- | Get the url for a given page.
toURL :: FilePath -> FilePath
toURL = flip addExtension ".html" . dropExtension

-- | Given a path to a file, try to make the path writable by making
--   all directories on the path.
makeDirectories :: FilePath -> IO ()
makeDirectories path = createDirectoryIfMissing True dir
    where dir = takeDirectory path

-- | Get all contents of a directory. Note that files starting with a dot (.)
--   will be ignored.
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter isProper names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)
    where isProper = not . (== '.') . head

-- | A filter that takes all file names with a given extension. Prefix the
--   extension with a dot:
--
--   > havingExtension ".markdown" ["index.markdown", "style.css"] == ["index.markdown"]
havingExtension :: String -> [FilePath] -> [FilePath]
havingExtension extension = filter ((==) extension . takeExtension)

-- | Perform an IO action on every file in a given directory.
directory :: (FilePath -> IO ()) -> FilePath -> IO ()
directory action dir = getRecursiveContents dir >>= mapM_ action

-- | Check if a cache file is still valid.
isCacheValid :: FilePath -> [FilePath] -> IO Bool
isCacheValid cache depends = doesFileExist cache >>= \exists ->
    if not exists then return False
                  else do dependsModified <- (mapM getModificationTime depends) >>= return . maximum
                          cacheModified <- getModificationTime cache
                          return (cacheModified >= dependsModified)
