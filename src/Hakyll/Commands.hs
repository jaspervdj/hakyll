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
    , watch 
    ) where


--------------------------------------------------------------------------------
import           System.Exit                (exitWith, ExitCode)
import           Control.Applicative
import           Control.Concurrent

--------------------------------------------------------------------------------
import qualified Hakyll.Check               as Check
import           Hakyll.Core.Configuration
import           Hakyll.Core.Logger         (Verbosity)
import           Hakyll.Core.Rules
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Runtime
import           Hakyll.Core.Util.File

--------------------------------------------------------------------------------
#ifdef PREVIEW_SERVER
import           Hakyll.Preview.Poll (watchUpdates)
import           Hakyll.Preview.Server
#endif


--------------------------------------------------------------------------------
-- | Build the site
build :: Configuration -> Verbosity -> Rules a -> IO ExitCode
build conf verbosity rules = fst <$> run conf verbosity rules

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
    watch' conf verbosity rules (server conf port)
#else
preview _ _ _ _ = previewServerDisabled
#endif


--------------------------------------------------------------------------------
-- | Watch and recompile for changes
watch :: Configuration -> Verbosity -> Rules a -> IO ()
watch conf verbosity rules = watch' conf verbosity rules (return ())

watch' :: Configuration -> Verbosity -> Rules a -> IO () -> IO ()
#ifdef PREVIEW_SERVER
watch' conf verbosity rules action = do
    watchUpdates conf update
    _ <- forkIO (action)
    putStrLn "Press <enter> to quit watching."
    loop
  where
    update = do
        (_, ruleSet) <- run conf verbosity rules
        return $ rulesPattern ruleSet

    loop = do
        line <- getLine
        if line == ""
            then putStrLn "Quitting..."
            else loop     
#else
watch' _ _ _ _ = watchServerDisabled
#endif


--------------------------------------------------------------------------------
-- | Rebuild the site
rebuild :: Configuration -> Verbosity -> Rules a -> IO ExitCode
rebuild conf verbosity rules =
    clean conf >> build conf verbosity rules

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
deploy :: Configuration -> IO ExitCode
deploy conf = deploySite conf conf


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

watchServerDisabled :: IO ()
watchServerDisabled =
    mapM_ putStrLn
      [ "WATCH SERVER"
      , ""
      , "The watch server is not enabled in the version of Hakyll. To"
      , "enable it, set the flag to True and recompile Hakyll."
      , "Alternatively, use an external tool to serve your site directory."
      ]

#endif
