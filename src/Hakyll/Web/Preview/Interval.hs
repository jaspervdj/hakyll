-- | Interval-based implementation of preview polling, for the platforms which
-- are not supported by inotify.
--
module Hakyll.Web.Preview.Interval
    ( previewPoll
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import System.Time (getClockTime)
import Data.Set (Set)
import qualified Data.Set as S
import System.Directory (getModificationTime)

import Hakyll.Core.Configuration
import Hakyll.Core.Identifier
import Hakyll.Core.ResourceProvider

-- | A preview thread that periodically recompiles the site.
--
previewPoll :: HakyllConfiguration  -- ^ Configuration
            -> Set Resource         -- ^ Resources to watch
            -> IO ()                -- ^ Action called when something changes
            -> IO ()                -- ^ Can block forever
previewPoll _ resources callback = do
    let files = map (toFilePath . unResource) $ S.toList resources
    time <- getClockTime
    loop files time
  where
    delay = 1000000
    loop files time = do
        threadDelay delay
        modified <- any (time <) <$> mapM getModificationTime files
        when modified callback
        loop files =<< getClockTime
