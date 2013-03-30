module Hakyll.Preview.Poll
    ( previewPoll
    ) where

--------------------------------------------------------------------------------
import           Control.Monad             (void)
import           Data.List                 (isPrefixOf)
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           System.Directory          (canonicalizePath)
import           System.FSNotify           (startManagerConf, watchTree,
                                            Event(..), WatchConfig(..))
import           System.IO.Error           (catchIOError)

--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration



--------------------------------------------------------------------------------
-- | A preview thread that recompiles the site when files change.
previewPoll :: Configuration  -- ^ Configuration
            -> IO [FilePath]  -- ^ Updating action
            -> IO ()          -- ^ Can block forever
previewPoll conf update = monitor =<< startManagerConf (Debounce 0.1)
  where
    path = decodeString $ providerDirectory conf
    monitor manager = do
        _ <- update
        ignore <- mapM getPath
                  [destinationDirectory, storeDirectory, tmpDirectory]
        watchTree manager path (predicate ignore) (\_ -> void update)
    getPath fn = catchIOError (canonicalizePath $ fn conf)
                              (const $ return $ fn conf)
    predicate ignore evt
        | isRemove evt = False
        | any (flip isPrefixOf $ eventPath evt) ignore == True = False
        | (ignoreFile conf) (eventPath evt) == True = False
        | otherwise = True

eventPath :: Event -> FilePath
eventPath (Added p _) = encodeString p
eventPath (Modified p _) = encodeString p
eventPath (Removed p _) = encodeString p

isRemove :: Event -> Bool
isRemove (Removed _ _) = True
isRemove _ = False
