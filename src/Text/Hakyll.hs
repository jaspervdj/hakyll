module Text.Hakyll
    ( defaultHakyllConfiguration
    , hakyll
    , hakyllWithConfiguration
    ) where

import Control.Monad.Reader (runReaderT, liftIO)
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
    , siteDirectory = "_site"
    , cacheDirectory = "_cache"
    }

-- | Hakyll with a default configuration.
hakyll :: Hakyll () -> IO ()
hakyll = hakyllWithConfiguration defaultHakyllConfiguration

-- | Main function to run hakyll with a configuration.
hakyllWithConfiguration :: HakyllConfiguration -> Hakyll () -> IO ()
hakyllWithConfiguration configuration buildFunction = do
    args <- getArgs
    let f = case args of ["build"]      -> buildFunction
                         ["clean"]      -> clean
                         ["preview", p] -> buildFunction >> server (read p)
                         ["preview"]    -> buildFunction >> server 8000
                         ["server", p]  -> server (read p)
                         ["server"]     -> server 8000
                         _              -> help
    runReaderT f configuration

-- | Clean up directories.
clean :: Hakyll ()
clean = do askHakyll siteDirectory >>= remove'
           askHakyll cacheDirectory >>= remove'
  where
    remove' dir = liftIO $ do putStrLn $ "Removing " ++ dir ++ "..."
                              exists <- doesDirectoryExist dir
                              when exists $ removeDirectoryRecursive dir

-- | Show usage information.
help :: Hakyll ()
help = liftIO $ do
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

server :: Integer -> Hakyll ()
server p = askHakyll siteDirectory >>= liftIO . simpleServer (fromIntegral p)
