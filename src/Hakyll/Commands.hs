--------------------------------------------------------------------------------
-- | Implementation of Hakyll commands: build, preview...
{-# LANGUAGE CPP #-}
module Hakyll.Commands
    ( build
    , check
    , clean
    , preview
    , rebuild
    , server
    , deploy
    ) where


--------------------------------------------------------------------------------
import           System.Exit                (ExitCode (ExitSuccess), exitWith)
import           System.Process             (system)


--------------------------------------------------------------------------------
import qualified Hakyll.Check               as Check
import           Hakyll.Core.Configuration
import           Hakyll.Core.Logger         (Verbosity)
import           Hakyll.Core.Rules
import           Hakyll.Core.Runtime
import           Hakyll.Core.Util.File


--------------------------------------------------------------------------------
#ifdef PREVIEW_SERVER
import           Control.Concurrent         (forkIO)
import qualified Data.Set                   as S
import           Hakyll.Core.Identifier
import           Hakyll.Core.Rules.Internal
import           Hakyll.Preview.Poll
import           Hakyll.Preview.Server
#endif


--------------------------------------------------------------------------------
-- | Build the site
build :: Configuration -> Verbosity -> Rules a -> IO ()
build conf verbosity rules = do
    _ <- run conf verbosity rules
    return ()


--------------------------------------------------------------------------------
-- | Run the checker and exit
check :: Configuration -> Verbosity -> Check.Check -> IO ()
check config verbosity check' = Check.check config verbosity check' >>= exitWith


--------------------------------------------------------------------------------
-- | Remove the output directories
clean :: Configuration -> IO ()
clean conf = do
    remove $ destinationDirectory conf
    remove $ storeDirectory conf
    remove $ tmpDirectory conf
  where
    remove dir = do
        putStrLn $ "Removing " ++ dir ++ "..."
        removeDirectory dir


--------------------------------------------------------------------------------
-- | Preview the site
preview :: Configuration -> Verbosity -> Rules a -> Int -> IO ()
#ifdef PREVIEW_SERVER
preview conf verbosity rules port = do
    -- Run the server in a separate thread
    _ <- forkIO $ server conf port
    previewPoll conf update
  where
    update = do
        (exitCode, ruleSet) <- run conf verbosity rules
        case exitCode of
            ExitSuccess -> return $ map toFilePath $ S.toList $
                rulesResources ruleSet
            _           -> exitWith exitCode
#else
preview _ _ _ _ = previewServerDisabled
#endif


--------------------------------------------------------------------------------
-- | Rebuild the site
rebuild :: Configuration -> Verbosity -> Rules a -> IO ()
rebuild conf verbosity rules = do
    clean conf
    build conf verbosity rules


--------------------------------------------------------------------------------
-- | Start a server
server :: Configuration -> Int -> IO ()
#ifdef PREVIEW_SERVER
server conf port = do
    let destination = destinationDirectory conf
    staticServer destination preServeHook port
  where
    preServeHook _ = return ()
#else
server _ _ = previewServerDisabled
#endif


--------------------------------------------------------------------------------
-- | Upload the site
deploy :: Configuration -> IO ()
deploy conf = do
    _ <- system $ deployCommand conf
    return ()


--------------------------------------------------------------------------------
-- | Print a warning message about the preview serving not being enabled
#ifndef PREVIEW_SERVER
previewServerDisabled :: IO ()
previewServerDisabled =
    mapM_ putStrLn
        [ "PREVIEW SERVER"
        , ""
        , "The preview server is not enabled in the version of Hakyll. To"
        , "enable it, set the flag to True and recompile Hakyll."
        , "Alternatively, use an external tool to serve your site directory."
        ]
#endif
