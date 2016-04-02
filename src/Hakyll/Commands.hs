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
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad              (void, forM_)
import           System.Exit                (ExitCode, exitWith)
import           System.IO.Error            (catchIOError)
import           System.Directory           (doesDirectoryExist, getDirectoryContents, removeFile)
import           System.FilePath            ((</>), pathSeparator)

--------------------------------------------------------------------------------
import qualified Hakyll.Check               as Check
import           Hakyll.Core.Configuration
import           Hakyll.Core.Logger         (Logger)
import qualified Hakyll.Core.Logger         as Logger
import           Hakyll.Core.Rules
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Runtime
import           Hakyll.Core.Util.File

--------------------------------------------------------------------------------
#ifdef WATCH_SERVER
import           Hakyll.Preview.Poll        (watchUpdates)
#endif

#ifdef PREVIEW_SERVER
import           Hakyll.Preview.Server
#endif


--------------------------------------------------------------------------------
-- | Build the site
build :: Configuration -> Logger -> Rules a -> IO ExitCode
build conf logger rules = fst <$> run conf logger rules


--------------------------------------------------------------------------------
-- | Run the checker and exit
check :: Configuration -> Logger -> Check.Check -> IO ()
check config logger check' = Check.check config logger check' >>= exitWith


--------------------------------------------------------------------------------
-- | Remove the output directories
clean :: Configuration -> Logger -> IO ()
clean conf logger = do
    removeGitFriendly $ destinationDirectory conf
    remove $ storeDirectory conf
    remove $ tmpDirectory conf
  where
    remove dir = do
        Logger.header logger $ "Removing " ++ dir ++ "..."
        removeDirectory dir

    removeGitFriendly dir = do
        Logger.header logger $ "Removing " ++ dir ++ " (keeping potential .git)..."
        containsGitRepo <- doesDirectoryExist (dir ++ [pathSeparator] ++ ".git")
        if containsGitRepo
            then do
                nestedFilePaths <- getNonRecursiveDirContents dir
                forM_ nestedFilePaths $ \nestedFilePath -> removeFilePath (dir </> nestedFilePath)
            else do
                remove dir

    getNonRecursiveDirContents dir = do
        names <- getDirectoryContents dir
        return $ filter (`notElem` [".", "..", ".git"]) names

    removeFilePath filePath = do
        isDirectory <- doesDirectoryExist filePath
        if isDirectory
            then removeDirectory filePath
            else removeFile filePath

--------------------------------------------------------------------------------
-- | Preview the site
preview :: Configuration -> Logger -> Rules a -> Int -> IO ()
#ifdef PREVIEW_SERVER
preview conf logger rules port  = do
    deprecatedMessage
    watch conf logger "0.0.0.0" port True rules
  where
    deprecatedMessage = mapM_ putStrLn [ "The preview command has been deprecated."
                                       , "Use the watch command for recompilation and serving."
                                       ]
#else
preview _ _ _ _ = previewServerDisabled
#endif


--------------------------------------------------------------------------------
-- | Watch and recompile for changes

watch :: Configuration -> Logger -> String -> Int -> Bool -> Rules a -> IO ()
#ifdef WATCH_SERVER
watch conf logger host port runServer rules = do
#ifndef mingw32_HOST_OS
    _ <- forkIO $ watchUpdates conf update
#else
    -- Force windows users to compile with -threaded flag, as otherwise
    -- thread is blocked indefinitely.
    catchIOError (void $ forkOS $ watchUpdates conf update) $ do
        fail $ "Hakyll.Commands.watch: Could not start update watching " ++
               "thread. Did you compile with -threaded flag?"
#endif
    server'
  where
    update = do
        (_, ruleSet) <- run conf logger rules
        return $ rulesPattern ruleSet
    loop = threadDelay 100000 >> loop
    server' = if runServer then server conf logger host port else loop
#else
watch _ _ _ _ _ _ = watchServerDisabled
#endif

--------------------------------------------------------------------------------
-- | Rebuild the site
rebuild :: Configuration -> Logger -> Rules a -> IO ExitCode
rebuild conf logger rules =
    clean conf logger >> build conf logger rules

--------------------------------------------------------------------------------
-- | Start a server
server :: Configuration -> Logger -> String -> Int -> IO ()
#ifdef PREVIEW_SERVER
server conf logger host port = do
    let destination = destinationDirectory conf
    staticServer logger destination preServeHook host port
  where
    preServeHook _ = return ()
#else
server _ _ _ _ = previewServerDisabled
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
#endif

#ifndef WATCH_SERVER
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
