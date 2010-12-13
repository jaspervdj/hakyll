-- | Module describing the Hakyll monad stack.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Hakyll.Monad
    ( HakyllConfiguration (..)
    , PreviewMode (..)
    , Hakyll
    , askHakyll
    , getAdditionalContext
    , logHakyll
    , forkHakyllWait
    , concurrentHakyll
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.MVar (MVar, putMVar, newEmptyMVar, readMVar)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad (liftM, forM, forM_)
import qualified Data.Map as M
import System.IO (hPutStrLn, stderr)

import Text.Pandoc (ParserState, WriterOptions)
import Text.Hamlet (HamletSettings)

import Text.Hakyll.Context (Context (..))

-- | Our custom monad stack.
--
newtype Hakyll a = Hakyll (ReaderT HakyllConfiguration IO a)
                 deriving (Monad, Functor)

instance MonadIO Hakyll where
    liftIO = Hakyll . liftIO

-- | Run a hakyll stack
--
runHakyll :: Hakyll a -> HakyllConfiguration -> IO a
runHakyll (Hakyll h) = runReaderT h

-- | Preview mode.
--
data PreviewMode = BuildOnRequest
                 | BuildOnInterval
                 deriving (Show, Eq, Ord)

-- | Hakyll global configuration type.
--
data HakyllConfiguration = HakyllConfiguration
    { -- | Absolute URL of the site.
      absoluteUrl         :: String
    , -- | An additional context to use when rendering. This additional context
      --   is used globally.
      additionalContext   :: Context
    , -- | Directory where the site is placed.
      siteDirectory       :: FilePath
    , -- | Directory for cache files.
      cacheDirectory      :: FilePath
    , -- | Enable index links.
      enableIndexUrl      :: Bool
    , -- | The preview mode used
      previewMode         :: PreviewMode
    , -- | Pandoc parsing options
      pandocParserState   :: ParserState
    , -- | Pandoc writer options
      pandocWriterOptions :: WriterOptions
    , -- | Hamlet settings (if you use hamlet for templates)
      hamletSettings      :: HamletSettings
    }

-- | Get the hakyll configuration
--
getHakyllConfiguration :: Hakyll HakyllConfiguration
getHakyllConfiguration = Hakyll ask

-- | Simplified @ask@ function for the Hakyll monad stack.
--
--   Usage would typically be something like:
--
--   > doSomething :: a -> b -> Hakyll c
--   > doSomething arg1 arg2 = do
--   >     siteDirectory' <- askHakyll siteDirectory
--   >     ...
--
askHakyll :: (HakyllConfiguration -> a) -> Hakyll a
askHakyll = flip liftM getHakyllConfiguration

-- | Obtain the globally available, additional context.
--
getAdditionalContext :: HakyllConfiguration -> Context
getAdditionalContext configuration =
    let (Context c) = additionalContext configuration
    in Context $ M.insert "absolute" (absoluteUrl configuration) c

-- | Write some log information.
--
logHakyll :: String -> Hakyll ()
logHakyll = Hakyll . liftIO . hPutStrLn stderr

-- | Perform a concurrent hakyll action. Returns an MVar you can wait on
--
forkHakyllWait :: Hakyll () -> Hakyll (MVar ())
forkHakyllWait action = do
    mvar <- liftIO newEmptyMVar
    config <- getHakyllConfiguration
    liftIO $ do
        runHakyll action config
        putMVar mvar ()
    return mvar

-- | Perform a number of concurrent hakyll actions, and waits for them to finish
--
concurrentHakyll :: [Hakyll ()] -> Hakyll ()
concurrentHakyll actions = do
    mvars <- forM actions forkHakyllWait
    forM_ mvars (liftIO . readMVar)
