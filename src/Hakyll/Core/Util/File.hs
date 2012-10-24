-- | A module containing various file utility functions
--
module Hakyll.Core.Util.File
    ( makeDirectories
    , getRecursiveContents
    , isFileObsolete
    , isFileInternal
    ) where

import Control.Applicative ((<$>))
import Data.Time.Clock (UTCTime)
import Control.Monad (forM, filterM)
import Data.List (isPrefixOf)
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist
                        , doesFileExist, getModificationTime
                        , getDirectoryContents
                        )
import System.FilePath ( normalise, takeDirectory, splitPath
                       , dropTrailingPathSeparator, (</>)
                       )

import Hakyll.Core.Configuration

-- | Given a path to a file, try to make the path writable by making
--   all directories on the path.
--
makeDirectories :: FilePath -> IO ()
makeDirectories = createDirectoryIfMissing True . takeDirectory

-- | Get all contents of a directory. Note that files starting with a dot (.)
-- will be ignored.
--
getRecursiveContents :: Bool           -- ^ Include directories?
                     -> FilePath       -- ^ Directory to search
                     -> IO [FilePath]  -- ^ List of files found
getRecursiveContents includeDirs topdir = do
    topdirExists <- doesDirectoryExist topdir
    if not topdirExists
        then return []
        else do
            names <- filter isProper <$> getDirectoryContents topdir
            paths <- forM names $ \name -> do
                let path = normalise $ topdir </> name
                isDirectory <- doesDirectoryExist path
                if isDirectory then getRecursiveContents includeDirs path
                               else return [path]
            return $ if includeDirs then topdir : concat paths
                                    else concat paths
  where
    isProper = (`notElem` [".", ".."])

-- | Check if a timestamp is obsolete compared to the timestamps of a number of
-- files. When they are no files, it is never obsolete.
--
isObsolete :: UTCTime    -- ^ The time to check.
           -> [FilePath]   -- ^ Dependencies of the cached file.
           -> IO Bool
isObsolete _ [] = return False
isObsolete timeStamp depends = do
    depends' <- filterM doesFileExist depends
    dependsModified <- mapM getModificationTime depends'
    return (timeStamp < maximum dependsModified)

-- | Check if a file is obsolete, given it's dependencies. When the file does
-- not exist, it is always obsolete. Other wise, it is obsolete if any of it's
-- dependencies has a more recent modification time than the file.
--
isFileObsolete :: FilePath    -- ^ The cached file
               -> [FilePath]  -- ^ Dependencies of the cached file
               -> IO Bool
isFileObsolete file depends = do
    exists <- doesFileExist file
    if not exists
        then return True
        else do timeStamp <- getModificationTime file
                isObsolete timeStamp depends

-- | Check if a file is meant for Hakyll internal use, i.e. if it is located in
-- the destination or store directory
--
isFileInternal :: HakyllConfiguration  -- ^ Configuration
               -> FilePath             -- ^ File to check
               -> Bool                 -- ^ If the given file is internal
isFileInternal configuration file =
    any (`isPrefixOf` split file) dirs
  where
    split = map dropTrailingPathSeparator . splitPath
    dirs = map (split . ($ configuration)) [ destinationDirectory
                                           , storeDirectory
                                           ]
