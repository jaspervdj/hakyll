--------------------------------------------------------------------------------
-- | Implements a basic static file server for previewing options
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Preview.Server
    ( staticServer
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans   (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Snap.Core             as Snap
import qualified Snap.Http.Server      as Snap
import qualified Snap.Util.FileServe   as Snap


--------------------------------------------------------------------------------
import           Hakyll.Core.Logger    (Logger)
import qualified Hakyll.Core.Logger    as Logger


--------------------------------------------------------------------------------
-- | Serve a given directory
static :: FilePath             -- ^ Directory to serve
       -> (FilePath -> IO ())  -- ^ Pre-serve hook
       -> Snap.Snap ()
static directory preServe =
    Snap.serveDirectoryWith directoryConfig directory
  where
    directoryConfig :: Snap.DirectoryConfig Snap.Snap
    directoryConfig = Snap.fancyDirectoryConfig
        { Snap.preServeHook = liftIO . preServe
        }


--------------------------------------------------------------------------------
-- | Main method, runs a static server in the given directory
staticServer :: Logger               -- ^ Logger
             -> FilePath             -- ^ Directory to serve
             -> (FilePath -> IO ())  -- ^ Pre-serve hook
             -> String               -- ^ Host to bind on
             -> Int                  -- ^ Port to listen on
             -> IO ()                -- ^ Blocks forever
staticServer logger directory preServe host port = do
    Logger.header logger $ "Listening on http://" ++ host ++ ":" ++ show port
    Snap.httpServe config $ static directory preServe
  where
    -- Snap server config
    config = Snap.setBind  (B.pack host)
           $ Snap.setPort      port
           $ Snap.setAccessLog Snap.ConfigNoLog
           $ Snap.setErrorLog  Snap.ConfigNoLog
           $ Snap.setVerbose   False
           $ Snap.emptyConfig
