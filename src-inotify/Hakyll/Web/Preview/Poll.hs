-- | Filesystem polling with an inotify backend. Works only on linux.
--
module Hakyll.Web.Preview.Poll
    ( previewPoll
    ) where

import Control.Monad (forM_, when)
import Data.Set (Set)
import qualified Data.Set as S
import System.FilePath (takeDirectory, (</>))
import Data.List (isPrefixOf)

import System.INotify

import Hakyll.Core.Configuration
import Hakyll.Core.Resource
import Hakyll.Core.Identifier

-- | Calls the given callback when the directory tree changes
--
previewPoll :: HakyllConfiguration  -- ^ Configuration
            -> Set Resource         -- ^ Resources to watch
            -> IO ()                -- ^ Action called when something changes
            -> IO ()                -- ^ Can block forever
previewPoll _ resources callback = do
    -- Initialize inotify
    inotify <- initINotify

    let -- A set of file paths
        paths = S.map (toFilePath . unResource) resources

        -- A list of directories. Run it through a set so we have every
        -- directory only once.
        directories = S.toList $ S.map (notEmpty . takeDirectory) paths

        -- Problem: we can't add a watcher for "". So we make sure a directory
        -- name is not empty
        notEmpty "" = "."
        notEmpty x  = x

        -- Execute the callback when path is known
        ifResource path =
            let path' = if "./" `isPrefixOf` path then drop 2 path else path
            in when (path' `S.member` paths) callback

    -- Add a watcher for every directory
    forM_ directories $ \directory -> do
        _ <- addWatch inotify [Modify] directory $ \e -> case e of
            (Modified _ (Just p)) -> ifResource $ directory </> p
            _                     -> return ()
        return ()
