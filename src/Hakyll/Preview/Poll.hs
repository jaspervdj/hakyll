{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------
module Hakyll.Preview.Poll
    ( watchUpdates
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.MVar        (newMVar, putMVar, takeMVar)
import           Control.Monad                  (when)
import           Filesystem.Path.CurrentOS      (decodeString, encodeString)
import           System.Directory               (canonicalizePath)
import           System.FilePath                (pathSeparators, (</>))
import           System.FSNotify                (Event (..), WatchConfig (..),
                                                 startManagerConf, watchTree)

#ifdef mingw32_HOST_OS
import System.IO (IOMode(ReadMode), Handle, openFile, hClose)
import System.IO.Error (isPermissionError)
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, throw, try)
#endif

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
        when allowed' $ update' ((encodeString providerDir) </> (eventPath event))
        putMVar lock ()
    where
#ifndef mingw32_HOST_OS
      update' _ = update >> return ()
#else
      update' path = waitOpen path ReadMode (\_ -> update) >> return ()

      -- continuously attempts to open the file in between sleep intervals
      -- handler is run only once it is able to open the file
      waitOpen :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
      waitOpen path mode handler = do
          res <- try $ openFile path mode :: IO (Either IOException Handle)
          case res of
              Left ex -> if isPermissionError ex
                         then do
                             threadDelay 100000
                             waitOpen path mode handler
                         else throw ex
              Right h -> do
                  handled <- handler h
                  hClose h
                  return handled
#endif

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
