module Text.Hakyll
    ( defaultHakyllConfiguration
    , hakyll
    ) where

import Control.Monad.Reader (runReaderT)
import Control.Monad (when)
import qualified Data.Map as M
import System.Environment (getArgs, getProgName)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)

import Network.Hakyll.SimpleServer (simpleServer)
import Text.Hakyll.Hakyll

-- | Default hakyll configuration.
defaultHakyllConfiguration :: HakyllConfiguration
defaultHakyllConfiguration = HakyllConfiguration
    { additionalContext = M.empty
    }

-- | Main function to run hakyll.
hakyll :: HakyllConfiguration -> Hakyll () -> IO ()
hakyll configuration buildFunction = do
    args <- getArgs
    case args of ["build"]      -> build'
                 ["clean"]      -> clean
                 ["preview", p] -> build' >> server (read p)
                 ["preview"]    -> build' >> server 8000
                 ["server", p]  -> server (read p)
                 ["server"]     -> server 8000
                 _              -> help
  where
    build' = build configuration buildFunction

-- | Build the site.
build :: HakyllConfiguration -> Hakyll () -> IO ()
build configuration buildFunction = do putStrLn "Generating..."
                                       runReaderT buildFunction configuration

-- | Clean up directories.
clean :: IO ()
clean = remove' "_site"
  where
    remove' dir = do putStrLn $ "Removing " ++ dir ++ "..."
                     exists <- doesDirectoryExist dir
                     when exists $ removeDirectoryRecursive dir

-- | Show usage information.
help :: IO ()
help = do
    name <- getProgName
    putStrLn $  "This is a Hakyll site generator program. You should always\n"
             ++ "run it from the project root directory.\n"
             ++ "\n"
             ++ "Usage:\n"
             ++ name ++ " build           Generate the site.\n"
             ++ name ++ " clean           Clean up and remove cache.\n"
             ++ name ++ " help            Show this message.\n"
             ++ name ++ " preview [port]  Generate site, then start a server.\n"
             ++ name ++ " server [port]   Run a local test server.\n"

server :: Integer -> IO ()
server p = simpleServer (fromIntegral p) "_site"
