--------------------------------------------------------------------------------
-- | Interval-based implementation of preview polling
{-# LANGUAGE CPP #-}
module Hakyll.Preview.Poll
    ( previewPoll
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Concurrent        (threadDelay)
import           Control.Monad             (filterM)
#if MIN_VERSION_directory(1,2,0)
import           Data.Time                 (getCurrentTime)
#else
import           System.Time               (getClockTime)
#endif
import           System.Directory          (doesFileExist, getModificationTime)


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration


--------------------------------------------------------------------------------
-- | A preview thread that periodically recompiles the site.
previewPoll :: Configuration  -- ^ Configuration
            -> IO [FilePath]  -- ^ Updating action
            -> IO ()          -- ^ Can block forever
previewPoll _ update = do
#if MIN_VERSION_directory(1,2,0)
    time <- getCurrentTime
#else
    time <- getClockTime
#endif
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
