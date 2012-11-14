--------------------------------------------------------------------------------
-- | Produce pretty, thread-safe logs
module Hakyll.Core.Logger
    ( Verbosity (..)
    , Logger
    , new
    , flush
    , error
    , header
    , item
    , subitem
    , debug
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative     (pure, (<$>), (<*>))
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever)
import           Control.Monad.Trans     (MonadIO, liftIO)
import           Data.List               (intercalate)
import           Prelude                 hiding (error)


--------------------------------------------------------------------------------
data Verbosity
    = Error
    | Header
    | Message
    | Debug
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
-- | Logger structure. Very complicated.
data Logger = Logger
    { loggerChan      :: Chan (Maybe String)  -- ^ Nothing marks the end
    , loggerSync      :: MVar ()              -- ^ Used for sync on quit
    , loggerSink      :: String -> IO ()      -- ^ Out sink
    , loggerVerbosity :: Verbosity            -- ^ Verbosity
    , loggerColumns   :: Int                  -- ^ Preferred number of columns
    }


--------------------------------------------------------------------------------
-- | Create a new logger
new :: Verbosity -> (String -> IO ()) -> IO Logger
new vbty sink = do
    logger <- Logger <$>
        newChan <*> newEmptyMVar <*> pure sink <*> pure vbty <*> pure 80
    _      <- forkIO $ loggerThread logger
    return logger
  where
    loggerThread logger = forever $ do
        msg <- readChan $ loggerChan logger
        case msg of
            -- Stop: sync
            Nothing -> putMVar (loggerSync logger) ()
            -- Print and continue
            Just m  -> loggerSink logger m


--------------------------------------------------------------------------------
-- | Flush the logger (blocks until flushed)
flush :: Logger -> IO ()
flush logger = do
    writeChan (loggerChan logger) Nothing
    () <- takeMVar $ loggerSync logger
    return ()


--------------------------------------------------------------------------------
string :: MonadIO m
       => Logger     -- ^ Logger
       -> Verbosity  -- ^ Verbosity of the string
       -> String     -- ^ Section name
       -> m ()       -- ^ No result
string l v m
    | loggerVerbosity l >= v = liftIO $ writeChan (loggerChan l) (Just m)
    | otherwise              = return ()


--------------------------------------------------------------------------------
error :: MonadIO m => Logger -> String -> m ()
error l m = string l Error $ "ERROR: " ++ m


--------------------------------------------------------------------------------
header :: MonadIO m => Logger -> String -> m ()
header l = string l Header


--------------------------------------------------------------------------------
item :: MonadIO m => Logger -> [String] -> m ()
item = itemWith 2


--------------------------------------------------------------------------------
subitem :: MonadIO m => Logger -> [String] -> m ()
subitem = itemWith 4


--------------------------------------------------------------------------------
itemWith :: MonadIO m => Int -> Logger -> [String] -> m ()
itemWith _ _ []       = return ()
itemWith i l [x]      = string l Message $ replicate i ' ' ++ x
itemWith i l (x : ys) = string l Message $ indent ++ x ++ spaces ++ ys'
  where
    indent = replicate i ' '
    spaces = replicate (max 1 $ loggerColumns l - i - length x - length ys') ' '
    ys'    = intercalate ", " ys


--------------------------------------------------------------------------------
debug :: MonadIO m => Logger -> String -> m ()
debug l m = string l Debug $ "  DEBUG: " ++ m
