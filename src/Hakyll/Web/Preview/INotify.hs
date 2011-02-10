-- | Filesystem polling with an inotify backend. Works only on linux.
--
module Hakyll.Web.Preview.INotify
    ( previewPoll
    ) where

import Control.Monad (forM_, when, unless)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (isPrefixOf)

import System.INotify

import Hakyll.Core.Util.File
import Hakyll.Core.Configuration

-- | Calls the given callback when the directory tree changes
--
previewPoll :: HakyllConfiguration  -- ^ Configuration
            -> FilePath             -- ^ Root directory
            -> IO ()                -- ^ Action called when something changes
            -> IO ()                -- ^ Can block forever
previewPoll conf directory callback = do
    -- Initialize inotify
    inotify <- initINotify

    -- Start by watching all directories
    contents <- getRecursiveContents True directory
    forM_ contents $ \file -> do
        isDir <- doesDirectoryExist file
        when isDir $ watchDirectory conf inotify file callback

-- | Start watching a directory recursively: when another directory is created
-- inside this directory, start watching that one as well...
--
watchDirectory :: HakyllConfiguration  -- ^ Configuration
               -> INotify              -- ^ INotify handle
               -> FilePath             -- ^ Directory to watch
               -> IO ()                -- ^ Callback
               -> IO ()                -- ^ No result
watchDirectory conf inotify path callback =
    unless (isFileInternal conf path) $ do
        _ <- addWatch inotify interesting path $ \event -> do
            putStrLn $ "Triggered: " ++ show event
            callback' inotify path event
        return ()
  where
    callback' i p (Created True n) = watchDirectory conf i (p </> n) callback
    callback' _ _ (Created _ p)    = whenProper $ Just p
    callback' _ _ (Modified _ p)   = whenProper p
    callback' _ _ (MovedOut _ p _) = whenProper $ Just p
    callback' _ _ (MovedIn _ p _)  = whenProper $ Just p
    callback' _ _ (Deleted _ p)    = whenProper $ Just p
    callback' _ _ _                = return ()

    interesting = [Modify, Create, Move, Delete]

    -- Call the callback only for proper files
    whenProper Nothing  = return ()
    whenProper (Just f) = unless ("." `isPrefixOf` f) callback
