{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}
--------------------------------------------------------------------------------
-- | A module containing various file utility functions
module Hakyll.Core.Util.File
    ( makeDirectories
    , getRecursiveContents
    , removeDirectory
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent  (threadDelay)
import           Control.Exception   (SomeException, catch)
import           Control.Monad       (filterM, forM, when)
import           System.Directory    (createDirectoryIfMissing,
                                      doesDirectoryExist, getDirectoryContents,
                                      removeDirectoryRecursive, removePathForcibly)
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
#ifndef mingw32_HOST_OS
removeDirectory fp = do
    e <- doesDirectoryExist fp
    when e $ removeDirectoryRecursive fp
#else
-- Deleting files on Windows is unreliable. If a file/directory is open by a program (e.g. antivirus),
-- then removing related directories *quickly* may fail with strange messages.
-- See here for discussions:
--      https://github.com/haskell/directory/issues/96
--      https://github.com/haskell/win32/pull/129
--
-- The hacky solution is to retry deleting directories a few times, 
-- with a delay, on Windows only.
removeDirectory = retryWithDelay 10 . removePathForcibly
#endif


--------------------------------------------------------------------------------
-- | Retry an operation at most /n/ times (/n/ must be positive).
--   If the operation fails the /n/th time it will throw that final exception.
--   A delay of 100ms is introduced between every retry.
retryWithDelay :: Int -> IO a -> IO a
retryWithDelay i x 
    | i <= 0    = error "Hakyll.Core.Util.File.retry: retry count must be 1 or more"
    | i == 1    = x
    | otherwise = catch x $ \(_::SomeException) -> threadDelay 100 >> retryWithDelay (i-1) x

