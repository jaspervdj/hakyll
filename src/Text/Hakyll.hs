module Text.Hakyll
    ( hakyll
    ) where

import Network.Hakyll.SimpleServer (simpleServer)

import System.Environment (getArgs, getProgName)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)

-- | Main function to run hakyll.
hakyll :: IO () -> IO ()
hakyll buildFunction = do
    args <- getArgs
    case args of []             -> build buildFunction
                 ["clean"]      -> clean
                 ["preview", p] -> build buildFunction >> server (read p)
                 ["preview"]    -> build buildFunction >> server 8000
                 ["server", p]  -> server (read p)
                 ["server"]     -> server 8000
                 _              -> help

-- | Build the site.
build :: IO () -> IO ()
build buildFunction = do putStrLn "Generating..."
                         buildFunction

-- | Clean up directories.
clean :: IO ()
clean = do remove' "_cache"
           remove' "_site"
    where remove' dir = do putStrLn $ "Removing " ++ dir ++ "..."
                           exists <- doesDirectoryExist dir
                           if exists then removeDirectoryRecursive dir
                                     else return ()

-- | Show usage information.
help :: IO ()
help = do
    name <- getProgName
    putStrLn $  "This is a hakyll site generator program. You should always\n"
             ++ "run it from the project root directory.\n"
             ++ "\n"
             ++ "Usage:\n"
             ++ name ++ "                 Generate the site.\n"
             ++ name ++ " clean           Clean up and remove cache.\n"
             ++ name ++ " help            Show this message.\n"
             ++ name ++ " preview [port]  Generate site, then start a server.\n"
             ++ name ++ " server [port]   Run a local test server.\n"

server :: Integer -> IO ()
server p = do simpleServer (fromIntegral $ p) "_site"
