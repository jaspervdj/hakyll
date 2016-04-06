--------------------------------------------------------------------------------
-- | A module containing various file utility functions
module Hakyll.Core.Util.File
    ( makeDirectories
    , getRecursiveContents
    , removeDirectory
    ) where


--------------------------------------------------------------------------------
import           Control.Monad       (filterM, forM, when)
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
getRecursiveContents :: (FilePath -> IO Bool)  -- ^ Ignore this file/directory
                     -> FilePath               -- ^ Directory to search
                     -> IO [FilePath]          -- ^ List of files found
getRecursiveContents ignore top = go ""
  where
    isProper x
        | x `elem` [".", ".."] = return False
        | otherwise            = not <$> ignore x

    go dir     = do
        dirExists <- doesDirectoryExist (top </> dir)
        if not dirExists
            then return []
            else do
                names <- filterM isProper =<< getDirectoryContents (top </> dir)
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
