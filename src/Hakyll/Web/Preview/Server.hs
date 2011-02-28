-- | Implements a basic static file server for previewing options
--
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Preview.Server
    ( staticServer
    ) where

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import Codec.Binary.UTF8.String
import System.FilePath ((</>))
import System.Directory (doesFileExist)

import qualified Data.ByteString as SB
import Snap.Util.FileServe (serveFile)
import Snap.Types (Snap, rqURI, getRequest, writeBS)
import Snap.Http.Server ( httpServe, setAccessLog, setErrorLog, addListen
                        , ConfigListen (..), emptyConfig
                        )

import Hakyll.Core.Util.String (replaceAll)

-- | The first file in the list that actually exists is returned
--
findFile :: [FilePath] -> IO (Maybe FilePath)
findFile [] = return Nothing
findFile (x : xs) = do
    exists <- doesFileExist x
    if exists then return (Just x) else findFile xs

-- | Serve a given directory
--
static :: FilePath             -- ^ Directory to serve
       -> (FilePath -> IO ())  -- ^ Pre-serve hook
       -> Snap ()
static directory preServe = do
    -- Obtain the path
    uri <- rqURI <$> getRequest
    let filePath = replaceAll "\\?$"    (const "")  -- Remove trailing ?
                 $ replaceAll "#[^#]*$" (const "")  -- Remove #section
                 $ replaceAll "^/"      (const "")  -- Remove leading /
                 $ decode $ SB.unpack uri

    -- Try to find the requested file
    r <- liftIO $ findFile $ map (directory </>) $
        [ filePath
        , filePath </> "index.htm"
        , filePath </> "index.html"
        ]

    case r of
        -- Not found, error
        Nothing -> writeBS "Not found"
        -- Found, serve
        Just f  -> do
            liftIO $ preServe f
            serveFile f

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
    config = addListen (ListenHttp "0.0.0.0" port)
           $ setAccessLog Nothing
           $ setErrorLog Nothing
           $ emptyConfig
