-- | Module containing various functions to manipulate contexts.
module Text.Hakyll.Context
    ( Context
    , ContextManipulation
    , renderValue
    , renderDate
    ) where

import qualified Data.Map as M
import Data.Map (Map)

import System.Locale (defaultTimeLocale)
import System.FilePath (takeFileName)
import Data.Time.Format (parseTime, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import Text.Hakyll.Regex (substituteRegex)

-- | Type for a context.
type Context = Map String String

-- | Type for context manipulating functions.
type ContextManipulation = Context -> Context

-- | Do something with a value of a context.
renderValue :: String -- ^ Key of which the value should be copied.
            -> String -- ^ Key the value should be copied to.
            -> (String -> String) -- ^ Function to apply on the value.
            -> ContextManipulation
renderValue src dst f context = case M.lookup src context of
    Nothing      -> context
    (Just value) -> M.insert dst (f value) context

-- | When the context has a key called `path` in a `yyyy-mm-dd-title.extension`
--   format (default for pages), this function can render the date.
renderDate :: String -- ^ Key in which the rendered date should be placed.
           -> String -- ^ Format to use on the date.
           -> String -- ^ Default value when the date cannot be parsed.
           -> ContextManipulation
renderDate key format defaultValue context = M.insert key value context
    where value = fromMaybe defaultValue pretty
          pretty = do filePath <- M.lookup "path" context
                      let dateString = substituteRegex "^([0-9]*-[0-9]*-[0-9]*).*" "\\1"
                                                       (takeFileName filePath)
                      time <- parseTime defaultTimeLocale
                                        "%Y-%m-%d"
                                        dateString :: Maybe UTCTime
                      return $ formatTime defaultTimeLocale format time
