-- | Module providing the main hakyll function and command-line argument parsing
--
module Hakyll.Main
    ( hakyll
    , hakyllWith
    ) where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import System.Environment (getProgName, getArgs)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)

import Hakyll.Core.Configuration
import Hakyll.Core.Run
import Hakyll.Core.Rules
import Hakyll.Core.Rules.Internal
import Hakyll.Web.Preview.Poll
import Hakyll.Web.Preview.Server

-- | This usualy is the function with which the user runs the hakyll compiler
--
hakyll :: Rules -> IO ()
hakyll = hakyllWith defaultHakyllConfiguration

-- | A variant of 'hakyll' which allows the user to specify a custom
-- configuration
--
hakyllWith :: HakyllConfiguration -> Rules -> IO ()
hakyllWith configuration rules = do
    args <- getArgs
    case args of
        ["build"]      -> build configuration rules
        ["clean"]      -> clean configuration
        ["help"]       -> help
        ["preview"]    -> preview configuration rules 8000
        ["preview", p] -> preview configuration rules (read p)
        ["rebuild"]    -> rebuild configuration rules
        ["server"]     -> server configuration 8000
        ["server", p]  -> server configuration (read p)
        _              -> help

-- | Build the site
--
build :: HakyllConfiguration -> Rules -> IO ()
build configuration rules = do
    _ <- run configuration rules
    return ()

-- | Remove the output directories
--
clean :: HakyllConfiguration -> IO ()
clean configuration = do
    remove $ destinationDirectory configuration
    remove $ storeDirectory configuration
  where
    remove dir = do
        putStrLn $ "Removing " ++ dir ++ "..."
        exists <- doesDirectoryExist dir
        when exists $ removeDirectoryRecursive dir

-- | Show usage information.
--
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
        ]

-- | Preview the site
--
preview :: HakyllConfiguration -> Rules -> Int -> IO ()
preview configuration rules port = do
    -- Build once, keep the rule set
    ruleSet <- run configuration rules

    -- Get the resource list and a callback for the preview poll
    let resources = rulesResources ruleSet
        callback = build configuration rules

    -- Fork a thread polling for changes
    _ <- forkIO $ previewPoll configuration resources callback
    
    -- Run the server in the main thread
    server configuration port

-- | Rebuild the site
--
rebuild :: HakyllConfiguration -> Rules -> IO ()
rebuild configuration rules = do
    clean configuration
    build configuration rules

-- | Start a server
--
server :: HakyllConfiguration -> Int -> IO ()
server configuration port = do
    let destination = destinationDirectory configuration
    staticServer destination preServeHook port
  where
    preServeHook _ = return ()
