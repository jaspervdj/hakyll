-- | Filesystem polling with an inotify backend. Works only on linux.
--
module Hakyll.Web.Preview.Poll
    ( previewPoll
    ) where

import Control.Monad (forM_, when)
import System.FilePath (takeDirectory, (</>))
import Data.List (isPrefixOf, nub)
import Data.Set (Set,toList)

import System.INotify

import Hakyll.Core.Configuration
import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier
import Debug.Trace

-- | Calls the given callback when the directory tree changes
--
previewPoll :: HakyllConfiguration  -- ^ Configuration
            -> Set Resource         -- ^ Resources to watch
            -> IO ()                -- ^ Action called when something changes
            -> IO ()                -- ^ Can block forever
previewPoll _ resources callback = do
    -- Initialize inotify
    inotify <- initINotify

    let -- Problem: we can't add a watcher for "". So we make sure a directory
        -- name is not empty
        notEmpty "" = "."
        notEmpty x  = x

        -- A list of directories. Run it through a set so we have every
        -- directory only once.
        directories = nub . map (notEmpty . takeDirectory . toFilePath . unResource) $ toList resources

    -- Add a watcher for every directory
    forM_ directories $ \directory -> addWatch inotify [MoveIn,MoveOut,Delete,Create] directory $ const callback
