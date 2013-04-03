module Hakyll.Preview.Poll
    ( watchUpdates
    ) where

--------------------------------------------------------------------------------
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           System.FSNotify           (startManagerConf, watchTree,
                                            Event(..), WatchConfig(..))

--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration



--------------------------------------------------------------------------------
-- | A thread that watches for updates in a 'providerDirectory' and recompiles
-- a site as soon as any changes occur
watchUpdates :: Configuration -> IO () -> IO ()
watchUpdates conf update = do
    _ <- update
    manager <- startManagerConf (Debounce 0.1)
    watchTree manager path (not . isRemove) update'
  where
    path = decodeString $ providerDirectory conf
    update' evt = do
        ignore <- shouldIgnoreFile conf $ eventPath evt
        if ignore then return () else update


eventPath :: Event -> FilePath
eventPath evt = encodeString $ evtPath evt
  where
    evtPath (Added p _) = p
    evtPath (Modified p _) = p
    evtPath (Removed p _) = p


isRemove :: Event -> Bool
isRemove (Removed _ _) = True
isRemove _ = False
