-- | Filesystem polling with an inotify backend. Works only on linux.
--
module Hakyll.Web.Preview.Poll
    ( previewPoll
    ) where

import Control.Monad (forM_)
import System.FilePath (takeDirectory)
import Data.List (nub)
import Data.Set (Set,toList)

import System.INotify (initINotify,addWatch,EventVariety (AllEvents), Event (..))

import Hakyll.Core.Configuration
import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier

-- | Calls the given callback when the directory tree changes
--
previewPoll :: HakyllConfiguration  -- ^ Configuration
            -> Set Resource         -- ^ Resources to watch
            -> IO ()                -- ^ Action called when something changes or added
            -> (FilePath -> IO ())                -- ^ Action called when something is deleted
            -> IO ()                -- ^ Can block forever
previewPoll _ resources build rebuild = do
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
    forM_ directories $ \directory -> addWatch inotify [AllEvents] directory $ \e -> case e of
	Created False _ -> build
	Modified False _ -> build
	MovedIn False _ _ -> build
	MovedOut False f _ -> rebuild f
	Deleted False f -> rebuild f
	x -> return ()	
