--------------------------------------------------------------------------------
-- | Module providing the main hakyll function and command-line argument parsing
{-# LANGUAGE CPP #-}
module Hakyll.Main
    ( hakyll
    , hakyllWith
    ) where


--------------------------------------------------------------------------------
import           Control.Monad              (when)
import           System.Directory           (doesDirectoryExist,
                                             removeDirectoryRecursive)
import           System.Environment         (getArgs, getProgName)
import           System.Process             (system)


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration
import           Hakyll.Core.Rules
import           Hakyll.Core.Runtime


--------------------------------------------------------------------------------
#ifdef PREVIEW_SERVER
import           Control.Applicative        ((<$>))
import           Control.Concurrent         (forkIO)
import qualified Data.Set                   as S
import           Hakyll.Core.Identifier
import           Hakyll.Core.Rules.Internal
import           Hakyll.Web.Preview.Poll
import           Hakyll.Web.Preview.Server
#endif


--------------------------------------------------------------------------------
-- | This usualy is the function with which the user runs the hakyll compiler
hakyll :: Rules a -> IO ()
hakyll = hakyllWith defaultConfiguration


--------------------------------------------------------------------------------
-- | A variant of 'hakyll' which allows the user to specify a custom
-- configuration
hakyllWith :: Configuration -> Rules a -> IO ()
hakyllWith conf rules = do
    args <- getArgs
    case args of
        ["build"]      -> build conf rules
        ["clean"]      -> clean conf
        ["help"]       -> help
        ["preview"]    -> preview conf rules 8000
        ["preview", p] -> preview conf rules (read p)
        ["rebuild"]    -> rebuild conf rules
        ["server"]     -> server conf 8000
        ["server", p]  -> server conf (read p)
        ["deploy"]     -> deploy conf
        _              -> help


--------------------------------------------------------------------------------
-- | Build the site
build :: Configuration -> Rules a -> IO ()
build conf rules = do
    _ <- run conf rules
    return ()


--------------------------------------------------------------------------------
-- | Remove the output directories
clean :: Configuration -> IO ()
clean conf = do
    remove $ destinationDirectory conf
    remove $ storeDirectory conf
  where
    remove dir = do
        putStrLn $ "Removing " ++ dir ++ "..."
        exists <- doesDirectoryExist dir
        when exists $ removeDirectoryRecursive dir


--------------------------------------------------------------------------------
-- | Show usage information.
help :: IO ()
help = do
    name <- getProgName
    mapM_ putStrLn
        [ "ABOUT"
        , ""
        , "This is a Hakyll site generator program. You should always"
        , "run it from the project root directory."
        , ""
        , "USAGE"
        , ""
        , name ++ " build           Generate the site"
        , name ++ " clean           Clean up and remove cache"
        , name ++ " help            Show this message"
        , name ++ " preview [port]  Run a server and autocompile"
        , name ++ " rebuild         Clean up and build again"
        , name ++ " server [port]   Run a local test server"
        , name ++ " deploy          Upload/deploy your site"
        , ""
        ]

#ifndef PREVIEW_SERVER
    previewServerDisabled
#endif


--------------------------------------------------------------------------------
-- | Preview the site
preview :: Configuration -> Rules a -> Int -> IO ()
#ifdef PREVIEW_SERVER
preview conf rules port = do
    -- Fork a thread polling for changes
    _ <- forkIO $ previewPoll conf update

    -- Run the server in the main thread
    server conf port
  where
    update = map toFilePath . S.toList . rulesResources <$> run conf rules
#else
preview _ _ _ = previewServerDisabled
#endif


--------------------------------------------------------------------------------
-- | Rebuild the site
rebuild :: Configuration -> Rules a -> IO ()
rebuild conf rules = do
    clean conf
    build conf rules


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
