--------------------------------------------------------------------------------
-- | Implements a basic static file server for previewing options
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Preview.Server
    ( staticServer
    ) where


--------------------------------------------------------------------------------
import           Data.String
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai                    as Wai
import           Network.HTTP.Types.Status      (Status)

--------------------------------------------------------------------------------
import           Hakyll.Core.Logger    (Logger)
import qualified Hakyll.Core.Logger    as Logger

staticServer :: Logger               -- ^ Logger
             -> Static.StaticSettings -- ^ Static file server settings
             -> String               -- ^ Host to bind on
             -> Int                  -- ^ Port to listen on
             -> IO ()                -- ^ Blocks forever
staticServer logger settings host port = do
    Logger.header logger $ "Listening on http://" ++ host ++ ":" ++ show port
    Logger.flush logger -- ensure this line is logged before Warp errors
    Warp.runSettings warpSettings $ Static.staticApp settings
  where
    warpSettings = Warp.setLogger noLog
        $ Warp.setHost (fromString host)
        $ Warp.setPort port Warp.defaultSettings

noLog :: Wai.Request -> Status -> Maybe Integer -> IO ()
noLog _ _ _ = return ()
