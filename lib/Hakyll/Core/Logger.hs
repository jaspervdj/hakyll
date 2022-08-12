--------------------------------------------------------------------------------
-- | Produce pretty, thread-safe logs
{-# LANGUAGE Rank2Types #-}
module Hakyll.Core.Logger
    ( Verbosity (..)
    , Logger
    , new
    , flush
    , error
    , header
    , message
    , debug

    -- * Testing utilities
    , newInMem
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (newChan, readChan, writeChan)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever, when)
import           Control.Monad.Trans     (MonadIO, liftIO)
import qualified Data.IORef              as IORef
import           Data.List               (intercalate)
import           Prelude                 hiding (error)


--------------------------------------------------------------------------------
data Verbosity
    = Error
    | Message
    | Debug
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data Logger = Logger
    { -- | Flush the logger (blocks until flushed)
      flush  :: forall m. MonadIO m => m ()
    , string :: forall m. MonadIO m => Verbosity -> String -> m ()
    }


--------------------------------------------------------------------------------
-- | Create a new logger
new :: Verbosity -> IO Logger
new vbty = do
    chan <- newChan
    sync <- newEmptyMVar
    _    <- forkIO $ forever $ do
        msg <- readChan chan
        case msg of
            -- Stop: sync
            Nothing -> putMVar sync ()
            -- Print and continue
            Just m  -> putStrLn m
    return $ Logger
        { flush = liftIO $ do
            writeChan chan Nothing
            () <- takeMVar sync
            return ()
        , string = \v m -> when (vbty >= v) $
            liftIO $ writeChan chan (Just m)
        }


--------------------------------------------------------------------------------
error :: MonadIO m => Logger -> String -> m ()
error l m = string l Error $ "  [ERROR] " ++ indent m


--------------------------------------------------------------------------------
header :: MonadIO m => Logger -> String -> m ()
header l = string l Message


--------------------------------------------------------------------------------
message :: MonadIO m => Logger -> String -> m ()
message l m = string l Message $ "  " ++ indent m


--------------------------------------------------------------------------------
debug :: MonadIO m => Logger -> String -> m ()
debug l m = string l Debug $ "  [DEBUG] " ++ indent m


--------------------------------------------------------------------------------
indent :: String -> String
indent = intercalate "\n    " . lines


--------------------------------------------------------------------------------
-- | Create a new logger that just stores all the messages, useful for writing
-- tests.
newInMem :: IO (Logger, IO [(Verbosity, String)])
newInMem = do
    ref <- IORef.newIORef []
    pure
        ( Logger
            { string = \vbty msg -> liftIO $ IORef.atomicModifyIORef' ref $
                \msgs -> ((vbty, msg) : msgs, ())
            , flush  = pure ()
            }
        , reverse <$> IORef.readIORef ref
        )
