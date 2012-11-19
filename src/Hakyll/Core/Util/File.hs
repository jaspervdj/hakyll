--------------------------------------------------------------------------------
-- | A module containing various file utility functions
module Hakyll.Core.Util.File
    ( makeDirectories
    , getRecursiveContents
    , isFileInternal
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Monad             (forM)
import           Data.List                 (isPrefixOf)
import           System.Directory          (createDirectoryIfMissing,
                                            doesDirectoryExist,
                                            getDirectoryContents)
import           System.FilePath           (dropTrailingPathSeparator,
                                            splitPath, takeDirectory, (</>))


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration


--------------------------------------------------------------------------------
-- | Given a path to a file, try to make the path writable by making
--   all directories on the path.
makeDirectories :: FilePath -> IO ()
makeDirectories = createDirectoryIfMissing True . takeDirectory


--------------------------------------------------------------------------------
-- | Get all contents of a directory.
getRecursiveContents :: FilePath       -- ^ Directory to search
                     -> IO [FilePath]  -- ^ List of files found
getRecursiveContents top = go ""
  where
    isProper = (`notElem` [".", ".."])
    go dir   = do
        dirExists <- doesDirectoryExist (top </> dir)
        if not dirExists
            then return []
            else do
                names <- filter isProper <$> getDirectoryContents (top </> dir)
                paths <- forM names $ \name -> do
                    let rel = dir </> name
                    isDirectory <- doesDirectoryExist (top </> rel)
                    if isDirectory
                        then go rel
                        else return [rel]

                return $ concat paths


--------------------------------------------------------------------------------
-- | Check if a file is meant for Hakyll internal use, i.e. if it is located in
-- the destination or store directory
isFileInternal :: Configuration  -- ^ Configuration
               -> FilePath       -- ^ File to check
               -> Bool                 -- ^ If the given file is internal
isFileInternal configuration file =
    any (`isPrefixOf` split file) dirs
  where
    split = map dropTrailingPathSeparator . splitPath
    dirs = map (split . ($ configuration)) [ destinationDirectory
                                           , storeDirectory
                                           ]
