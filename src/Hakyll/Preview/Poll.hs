--------------------------------------------------------------------------------
module Hakyll.Preview.Poll
    ( watchUpdates
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.MVar        (newMVar, putMVar, takeMVar)
import           Control.Monad                  (when)
import           Filesystem.Path.CurrentOS      (decodeString, encodeString)
import           System.Directory               (canonicalizePath)
import           System.FilePath                (pathSeparators)
import           System.FSNotify                (Event (..), WatchConfig (..),
                                                 startManagerConf, watchTree)


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern


--------------------------------------------------------------------------------
-- | A thread that watches for updates in a 'providerDirectory' and recompiles
-- a site as soon as any changes occur
watchUpdates :: Configuration -> IO Pattern -> IO ()
watchUpdates conf update = do
    let providerDir = decodeString $ providerDirectory conf
    lock            <- newMVar ()
    pattern         <- update
    fullProviderDir <- canonicalizePath $ providerDirectory conf
    manager         <- startManagerConf (Debounce 0.1)

    let allowed event = do
            -- Absolute path of the changed file. This must be inside provider
            -- dir, since that's the only dir we're watching.
            let path       = eventPath event
                relative   = dropWhile (`elem` pathSeparators) $
                    drop (length fullProviderDir) path
                identifier = fromFilePath relative

            shouldIgnore <- shouldIgnoreFile conf path
            return $ not shouldIgnore && matches pattern identifier

    watchTree manager providerDir (not . isRemove) $ \event -> do
        ()       <- takeMVar lock
        allowed' <- allowed event
        when allowed' $ update >> return ()
        putMVar lock ()


--------------------------------------------------------------------------------
eventPath :: Event -> FilePath
eventPath evt = encodeString $ evtPath evt
  where
    evtPath (Added p _)    = p
    evtPath (Modified p _) = p
    evtPath (Removed p _)  = p


--------------------------------------------------------------------------------
isRemove :: Event -> Bool
isRemove (Removed _ _) = True
isRemove _             = False
