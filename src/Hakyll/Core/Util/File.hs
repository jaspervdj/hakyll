--------------------------------------------------------------------------------
-- | A module containing various file utility functions
module Hakyll.Core.Util.File
    ( makeDirectories
    , getRecursiveContents
    , removeDirectory
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (forM, when)
import           System.Directory    (createDirectoryIfMissing,
                                      doesDirectoryExist, getDirectoryContents,
                                      removeDirectoryRecursive)
import           System.FilePath     (takeDirectory, (</>))


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
removeDirectory :: FilePath -> IO ()
removeDirectory fp = do
    e <- doesDirectoryExist fp
    when e $ removeDirectoryRecursive fp
