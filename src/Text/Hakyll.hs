-- | This is the main Hakyll module, exporting the important @hakyll@ function.
--
--   Most configurations would use this @hakyll@ function more or less as the
--   main function:
--
--   > main = hakyll $ do
--   >     directory css "css"
--   >     directory static "images"
--
module Text.Hakyll
    ( defaultHakyllConfiguration
    , hakyll
    , hakyllWithConfiguration
    ) where

import Control.Monad.Reader (runReaderT, liftIO, ask)
import Control.Monad (when)
import Data.Monoid (mempty)
import System.Environment (getArgs, getProgName)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)

import Text.Pandoc

import Network.Hakyll.SimpleServer (simpleServer)
import Text.Hakyll.HakyllMonad

-- | The default reader options for pandoc parsing.
--
defaultPandocParserState :: ParserState
defaultPandocParserState = defaultParserState
    { -- The following option causes pandoc to read smart typography, a nice
      -- and free bonus.
      stateSmart = True
    }

-- | The default writer options for pandoc rendering.
--
defaultPandocWriterOptions :: WriterOptions
defaultPandocWriterOptions = defaultWriterOptions
    { -- This option causes literate haskell to be written using '>' marks in
      -- html, which I think is a good default.
      writerLiterateHaskell = True
    }

-- | The default hakyll configuration.
--
defaultHakyllConfiguration :: String               -- ^ Absolute site URL.
                           -> HakyllConfiguration  -- ^ Default config.
defaultHakyllConfiguration absoluteUrl' = HakyllConfiguration
    { absoluteUrl         = absoluteUrl'
    , additionalContext   = mempty
    , siteDirectory       = "_site"
    , cacheDirectory      = "_cache"
    , enableIndexUrl      = False
    , pandocParserState   = defaultPandocParserState
    , pandocWriterOptions = defaultPandocWriterOptions
    }

-- | Main function to run Hakyll with the default configuration. The
-- absolute URL is only used in certain cases, for example RSS feeds et
-- cetera.
--
hakyll :: String    -- ^ Absolute URL of your site. Used in certain cases.
       -> Hakyll () -- ^ You code.
       -> IO ()
hakyll absolute = hakyllWithConfiguration configuration
  where
    configuration = defaultHakyllConfiguration absolute

-- | Main function to run hakyll with a custom configuration.
--
hakyllWithConfiguration :: HakyllConfiguration -> Hakyll () -> IO ()
hakyllWithConfiguration configuration buildFunction = do
    args <- getArgs
    let f = case args of ["build"]      -> buildFunction
                         ["clean"]      -> clean
                         ["preview", p] -> server (read p) buildFunction
                         ["preview"]    -> server 8000 buildFunction
                         ["rebuild"]    -> clean >> buildFunction
                         ["server", p]  -> server (read p) (return ())
                         ["server"]     -> server 8000 (return ())
                         _              -> help
    runReaderT f configuration

-- | Clean up directories.
--
clean :: Hakyll ()
clean = do askHakyll siteDirectory >>= remove'
           askHakyll cacheDirectory >>= remove'
  where
    remove' dir = liftIO $ do putStrLn $ "Removing " ++ dir ++ "..."
                              exists <- doesDirectoryExist dir
                              when exists $ removeDirectoryRecursive dir

-- | Show usage information.
--
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
             ++ name ++ " preview [port]  Run a server and autocompile.\n"
             ++ name ++ " rebuild         Clean up and build again.\n"
             ++ name ++ " server [port]   Run a local test server.\n"

-- | Start a server at the given port number.
--
server :: Integer    -- ^ Port number to serve on.
       -> Hakyll ()  -- ^ Pre-respond action.
       -> Hakyll ()
server port preRespond = do 
    configuration <- ask
    root <- askHakyll siteDirectory
    let preRespondIO = runReaderT preRespond configuration
    liftIO $ simpleServer (fromIntegral port) root preRespondIO
