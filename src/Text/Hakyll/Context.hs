-- | Module containing various functions to manipulate contexts.
module Text.Hakyll.Context
    ( ContextManipulation
    , renderValue
    , renderDate
    , ignoreKeys
    ) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B

import System.Locale (defaultTimeLocale)
import System.FilePath (takeFileName)
import Text.Template (Context)
import Data.Time.Format (parseTime, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import Text.Hakyll.Regex (substitute)

-- | Type for context manipulating functions.
type ContextManipulation = Context -> Context

-- | Do something with a value of a context.
renderValue :: String -- ^ Key of which the value should be copied.
            -> String -- ^ Key the value should be copied to.
            -> (B.ByteString -> B.ByteString) -- ^ Function to apply on the value.
            -> ContextManipulation
renderValue src dst f context = case M.lookup (B.pack src) context of
    Nothing      -> context
    (Just value) -> M.insert (B.pack dst) (f value) context

-- | When the context has a key called `path` in a `yyyy-mm-dd-title.extension`
--   format (default for pages), this function can render the date.
renderDate :: String -- ^ Key in which the rendered date should be placed.
           -> String -- ^ Format to use on the date.
           -> String -- ^ Default value when the date cannot be parsed.
           -> ContextManipulation
renderDate key format defaultValue context =
    M.insert (B.pack key) (B.pack value) context
    where value = fromMaybe defaultValue pretty
          pretty = do filePath <- M.lookup (B.pack "path") context
                      let dateString = substitute "^([0-9]*-[0-9]*-[0-9]*).*" "\\1"
                                                  (takeFileName $ B.unpack filePath)
                      time <- parseTime defaultTimeLocale
                                        "%Y-%m-%d"
                                        dateString :: Maybe UTCTime
                      return $ formatTime defaultTimeLocale format time

-- | Ignore a number of keys during the render phase.
ignoreKeys :: [String] -> ContextManipulation
ignoreKeys keyList = M.union (M.fromList pairs)
    where pairs = map pair keyList
          pair key = (B.pack $ '$' : key, B.pack $ '$' : key)
