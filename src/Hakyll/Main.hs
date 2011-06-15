-- | Module providing the main hakyll function and command-line argument parsing
--
module Hakyll.Main
    ( hakyll
    , hakyllWith
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (when)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Environment (getProgName, getArgs)
import System.Process (system)
import qualified Data.Set as S

import Hakyll.Core.Configuration
import Hakyll.Core.Resource
import Hakyll.Core.Run
import Hakyll.Core.Rules
import Hakyll.Core.Rules.Internal
import Hakyll.Web.Preview.Poll
import Hakyll.Web.Preview.Server

-- | This usualy is the function with which the user runs the hakyll compiler
--
hakyll :: RulesM a -> IO ()
hakyll = hakyllWith defaultHakyllConfiguration

-- | A variant of 'hakyll' which allows the user to specify a custom
-- configuration
--
hakyllWith :: HakyllConfiguration -> RulesM a -> IO ()
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

-- | Build the site
--
build :: HakyllConfiguration -> RulesM a -> IO ()
build conf rules = do
    _ <- run conf rules
    return ()

-- | Remove the output directories
--
clean :: HakyllConfiguration -> IO ()
clean conf = do
    remove $ destinationDirectory conf
    remove $ storeDirectory conf
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
        , name ++ " deploy          Upload/deploy your site"
        ]

-- | Preview the site
--
preview :: HakyllConfiguration -> RulesM a -> Int -> IO ()
preview conf rules port = do
    -- Fork a thread polling for changes
    _ <- forkIO $ previewPoll conf update
    
    -- Run the server in the main thread
    server conf port
  where
    update = map unResource . S.toList . rulesResources <$> run conf rules

-- | Rebuild the site
--
rebuild :: HakyllConfiguration -> RulesM a -> IO ()
rebuild conf rules = do
    clean conf
    build conf rules

-- | Start a server
--
server :: HakyllConfiguration -> Int -> IO ()
server conf port = do
    let destination = destinationDirectory conf
    staticServer destination preServeHook port
  where
    preServeHook _ = return ()

-- Upload the site
--
deploy :: HakyllConfiguration -> IO ()
deploy conf = do
    _ <- system $ deployCommand conf
    return ()
