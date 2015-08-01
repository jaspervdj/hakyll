--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Hakyll.Preview.Poll
    ( watchUpdates
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.MVar        (newEmptyMVar, takeMVar,
                                                 tryPutMVar)
import           Control.Exception              (AsyncException, fromException,
                                                 handle, throw)
import           Control.Monad                  (forever, void, when)
import           System.Directory               (canonicalizePath)
import           System.FilePath                (pathSeparators)
import           System.FSNotify                (Event (..), startManager,
                                                 watchTree)

#ifdef mingw32_HOST_OS
import           Control.Concurrent             (threadDelay)
import           Control.Exception              (IOException, throw, try)
import           System.Directory               (doesFileExist)
import           System.Exit                    (exitFailure)
import           System.FilePath                ((</>))
import           System.IO                      (Handle, IOMode (ReadMode),
                                                 hClose, openFile)
import           System.IO.Error                (isPermissionError)
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
    let providerDir = providerDirectory conf
    shouldBuild     <- newEmptyMVar
    pattern         <- update
    fullProviderDir <- canonicalizePath $ providerDirectory conf
    manager         <- startManager

    let allowed event = do
            -- Absolute path of the changed file. This must be inside provider
            -- dir, since that's the only dir we're watching.
            let path       = eventPath event
                relative   = dropWhile (`elem` pathSeparators) $
                    drop (length fullProviderDir) path
                identifier = fromFilePath relative

            shouldIgnore <- shouldIgnoreFile conf path
            return $ not shouldIgnore && matches pattern identifier

    -- This thread continually watches the `shouldBuild` MVar and builds
    -- whenever a value is present.
    _ <- forkIO $ forever $ do
        event <- takeMVar shouldBuild
        handle
            (\e -> case fromException e of
                Nothing    -> putStrLn (show e)
                Just async -> throw (async :: AsyncException))
            (update' event providerDir)

    -- Send an event whenever something occurs so that the thread described
    -- above will do a build.
    void $ watchTree manager providerDir (not . isRemove) $ \event -> do
        allowed' <- allowed event
        when allowed' $ void $ tryPutMVar shouldBuild event
  where
#ifndef mingw32_HOST_OS
    update' _     _        = void update
#else
    update' event provider = do
        let path = provider </> eventPath event
        -- on windows, a 'Modified' event is also sent on file deletion
        fileExists <- doesFileExist path

        when fileExists . void $ waitOpen path ReadMode (\_ -> update) 10

    -- continuously attempts to open the file in between sleep intervals
    -- handler is run only once it is able to open the file
    waitOpen :: FilePath -> IOMode -> (Handle -> IO r) -> Integer -> IO r
    waitOpen _    _    _       0 = do
        putStrLn "[ERROR] Failed to retrieve modified file for regeneration"
        exitFailure
    waitOpen path mode handler retries = do
        res <- try $ openFile path mode :: IO (Either IOException Handle)
        case res of
            Left ex -> if isPermissionError ex
                       then do
                           threadDelay 100000
                           waitOpen path mode handler (retries - 1)
                       else throw ex
            Right h -> do
                handled <- handler h
                hClose h
                return handled
#endif


--------------------------------------------------------------------------------
eventPath :: Event -> FilePath
eventPath evt = evtPath evt
  where
    evtPath (Added p _)    = p
    evtPath (Modified p _) = p
    evtPath (Removed p _)  = p


--------------------------------------------------------------------------------
isRemove :: Event -> Bool
isRemove (Removed _ _) = True
isRemove _             = False
