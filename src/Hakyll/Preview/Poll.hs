module Hakyll.Preview.Poll
    ( watchUpdates
    ) where

--------------------------------------------------------------------------------
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           System.Directory          (getCurrentDirectory)
import           System.FilePath           (makeRelative)
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
    workingDirectory <- getCurrentDirectory
    watchTree manager path (predicate workingDirectory) $ const update
  where
    path = decodeString $ providerDirectory conf
    predicate wDir evt
        | isRemove evt = False
        | otherwise = not $ shouldIgnoreFile conf (relativeEventPath wDir evt)


relativeEventPath :: FilePath -> Event -> FilePath
relativeEventPath b evt = makeRelative b $ encodeString $ evtPath evt
  where
    evtPath (Added p _) = p
    evtPath (Modified p _) = p
    evtPath (Removed p _) = p


isRemove :: Event -> Bool
isRemove (Removed _ _) = True
isRemove _ = False
