-- | Produce pretty, thread-safe logs
--
{-# LANGUAGE BangPatterns #-}
module Hakyll.Core.Logger
    ( Logger
    , makeLogger
    , flushLogger
    , section
    , timed
    , report
    , thrown
    ) where

import Control.Monad (forever)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Text.Printf (printf)

import Data.Time (getCurrentTime, diffUTCTime)

-- | Logger structure. Very complicated.
--
data Logger = Logger
    { loggerChan :: Chan (Maybe String)  -- ^ Nothing marks the end
    , loggerSync :: MVar ()              -- ^ Used for sync on quit
    , loggerSink :: String -> IO ()      -- ^ Out sink
    }

-- | Create a new logger
--
makeLogger :: (String -> IO ()) -> IO Logger
makeLogger sink = do
    logger <- Logger <$> newChan <*> newEmptyMVar <*> pure sink
    _ <- forkIO $ loggerThread logger
    return logger
  where
    loggerThread logger = forever $ do
        msg <- readChan $ loggerChan logger
        case msg of
            -- Stop: sync
            Nothing -> putMVar (loggerSync logger) ()
            -- Print and continue
            Just m  -> loggerSink logger m

-- | Flush the logger (blocks until flushed)
--
flushLogger :: Logger -> IO ()
flushLogger logger = do
    writeChan (loggerChan logger) Nothing
    () <- takeMVar $ loggerSync logger
    return ()

-- | Send a raw message to the logger
--
message :: Logger -> String -> IO ()
message logger = writeChan (loggerChan logger) . Just

-- | Start a section in the log
--
section :: MonadIO m
        => Logger  -- ^ Logger
        -> String  -- ^ Section name
        -> m ()    -- ^ No result
section logger = liftIO . message logger

-- | Execute a monadic action and log the duration
--
timed :: MonadIO m
      => Logger  -- ^ Logger
      -> String  -- ^ Message
      -> m a     -- ^ Action
      -> m a     -- ^ Timed and logged action
timed logger msg action = do
    start <- liftIO getCurrentTime
    !result <- action
    stop <- liftIO getCurrentTime
    let diff = fromEnum $ diffUTCTime stop start
        ms = diff `div` 10 ^ (9 :: Int)
        formatted = printf "  [%4dms] %s" ms msg
    liftIO $ message logger formatted
    return result

-- | Log something at the same level as 'timed', but without the timing
--
report :: MonadIO m
       => Logger  -- ^ Logger
       -> String  -- ^ Message
       -> m ()    -- ^ No result
report logger msg = liftIO $ message logger $ "  [      ] " ++ msg

-- | Log an error that was thrown in the compilation phase
--
thrown :: MonadIO m
       => Logger  -- ^ Logger
       -> String  -- ^ Message
       -> m ()    -- ^ No result
thrown logger msg = liftIO $ message logger $ "  [ ERROR] " ++ msg
