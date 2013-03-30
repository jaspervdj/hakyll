module Hakyll.Preview.Poll
    ( previewPoll
    ) where

--------------------------------------------------------------------------------
import           Control.Monad             (void)
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           System.Directory          (getCurrentDirectory)
import           System.FilePath           (makeRelative)
import           System.FSNotify           (startManagerConf, watchTree,
                                            Event(..), WatchConfig(..))

--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration



--------------------------------------------------------------------------------
-- | A preview thread that recompiles the site when files change.
previewPoll :: Configuration  -- ^ Configuration
            -> IO [FilePath]  -- ^ Updating action
            -> IO ()          -- ^ Can block forever
previewPoll conf update = do
    _ <- update
    manager <- startManagerConf (Debounce 0.1)
    wDir <- getCurrentDirectory
    watchTree manager path (predicate wDir) (\_ -> void update)
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
