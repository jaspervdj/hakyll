-- | A module containing various file utility functions
--
module Hakyll.Core.Util.File
    ( makeDirectories
    , getRecursiveContents
    , isFileInternal
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.List (isPrefixOf)
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist
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
