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
    , runDefaultHakyll
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Reader (runReaderT, liftIO, ask)
import Control.Monad (when)
import Data.Monoid (mempty)
import System.Environment (getArgs, getProgName)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Time (getClockTime)

import Text.Pandoc

import Network.Hakyll.SimpleServer (simpleServer)
import Text.Hakyll.HakyllMonad
import Text.Hakyll.File

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
    , previewMode         = BuildOnRequest
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
                         ["preview", p] -> preview (read p) 
                         ["preview"]    -> preview defaultPort
                         ["rebuild"]    -> clean >> buildFunction
                         ["server", p]  -> server (read p) (return ())
                         ["server"]     -> server defaultPort (return ())
                         _              -> help
    runReaderT f configuration
  where
    preview port = case previewMode configuration of
        BuildOnRequest  -> server port buildFunction
        BuildOnInterval -> do
            let pIO = runReaderT (previewThread buildFunction) configuration
            _ <- liftIO $ forkIO pIO
            server port (return ())

    defaultPort = 8000

-- | A preview thread that periodically recompiles the site.
--
previewThread :: Hakyll ()  -- ^ Build function
              -> Hakyll ()  -- ^ Result
previewThread buildFunction = run =<< liftIO getClockTime
  where
    delay = 1000000
    run time = do liftIO $ threadDelay delay
                  contents <- getRecursiveContents "."
                  valid <- isMoreRecent time contents
                  when valid buildFunction
                  run =<< liftIO getClockTime

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

-- | Run a Hakyll action with default settings. This is mostly aimed at testing
-- code.
--
runDefaultHakyll :: Hakyll a -> IO a
runDefaultHakyll f =
    runReaderT f $ defaultHakyllConfiguration "http://example.com"
