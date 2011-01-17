-- | Implements a basic static file server for previewing options
--
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import Codec.Binary.UTF8.String
import System.FilePath ((</>))
import System.Directory (doesFileExist)

import qualified Data.ByteString as SB
import Snap.Util.FileServe
import Snap.Types
import Snap.Http.Server

import Hakyll.Web.Util.String (replaceAll)

-- | The first file in the list that actually exists is returned
--
findFile :: [FilePath] -> IO (Maybe FilePath)
findFile [] = return Nothing
findFile (x : xs) = do
    exists <- doesFileExist x
    if exists then return (Just x) else findFile xs

-- | Serve a given directory
--
site :: FilePath             -- ^ Directory to serve
     -> (FilePath -> IO ())  -- ^ Pre-serve hook
     -> Snap ()
site directory preServe = do
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
            fileServeSingle f

-- | Main method, runs snap
--
main :: IO ()
main = httpServe defaultConfig $ site "." (\f -> putStrLn $ "Serving " ++ f)
