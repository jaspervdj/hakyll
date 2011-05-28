-- | Interval-based implementation of preview polling
--
module Hakyll.Web.Preview.Poll
    ( previewPoll
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (filterM)
import System.Time (getClockTime)
import System.Directory (getModificationTime, doesFileExist)

import Hakyll.Core.Configuration

-- | A preview thread that periodically recompiles the site.
--
previewPoll :: HakyllConfiguration  -- ^ Configuration
            -> IO [FilePath]        -- ^ Updating action
            -> IO ()                -- ^ Can block forever
previewPoll _ update = do
    time <- getClockTime
    loop time =<< update
  where
    delay = 1000000
    loop time files = do
        threadDelay delay
        files' <- filterM doesFileExist files
        filesTime <- case files' of
            []  -> return time
            _   -> maximum <$> mapM getModificationTime files'

        if filesTime > time || files' /= files
            then loop filesTime =<< update
            else loop time files'
