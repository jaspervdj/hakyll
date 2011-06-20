-- | Implements a basic static file server for previewing options
--
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Preview.Server
    ( staticServer
    ) where

import Control.Monad.Trans (liftIO)

import Snap.Types (Snap)
import Snap.Util.FileServe ( DirectoryConfig (..), fancyDirectoryConfig
                           , serveDirectoryWith
                           )
import Snap.Http.Server ( httpServe, setAccessLog, setErrorLog
                        , setPort, emptyConfig
                        )

-- | Serve a given directory
--
static :: FilePath             -- ^ Directory to serve
       -> (FilePath -> IO ())  -- ^ Pre-serve hook
       -> Snap ()
static directory preServe =
    serveDirectoryWith directoryConfig directory
  where
    directoryConfig :: DirectoryConfig Snap
    directoryConfig = fancyDirectoryConfig
        { preServeHook = liftIO . preServe
        }

-- | Main method, runs a static server in the given directory
--
staticServer :: FilePath             -- ^ Directory to serve
             -> (FilePath -> IO ())  -- ^ Pre-serve hook
             -> Int                  -- ^ Port to listen on
             -> IO ()                -- ^ Blocks forever
staticServer directory preServe port =
    httpServe config $ static directory preServe
  where
    -- Snap server config
    config = setPort port
           $ setAccessLog Nothing
           $ setErrorLog Nothing
           $ emptyConfig
