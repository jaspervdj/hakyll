-- | Module containing various functions to manipulate contexts.
module Text.Hakyll.Context
    ( Context
    , ContextManipulation
    , renderValue
    , changeValue
    , renderDate
    , changeExtension
    ) where

import qualified Data.Map as M
import Data.Map (Map)
import System.Locale (defaultTimeLocale)
import System.FilePath (takeFileName, addExtension, dropExtension)
import Data.Time.Format (parseTime, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)

import Text.Hakyll.Regex (substituteRegex)

-- | Type for a context.
type Context = Map String String

-- | Type for context manipulating functions.
type ContextManipulation = Context -> Context

-- | Do something with a value in a @Context@, but keep the old value as well.
--   This is probably the most common function to construct a
--   @ContextManipulation@.
renderValue :: String -- ^ Key of which the value should be copied.
            -> String -- ^ Key the value should be copied to.
            -> (String -> String) -- ^ Function to apply on the value.
            -> ContextManipulation
renderValue src dst f context = case M.lookup src context of
    Nothing      -> context
    (Just value) -> M.insert dst (f value) context

-- | Change a value in a @Context@.
--
--   > import Data.Char (toUpper)
--   > changeValue "title" (map toUpper)
--
--   Will put the title in UPPERCASE.
changeValue :: String -- ^ Key of which the value should be changed.
            -> (String -> String) -- ^ Function to apply on the value.
            -> ContextManipulation
changeValue key = renderValue key key

-- | When the context has a key called @path@ in a @yyyy-mm-dd-title.extension@
--   format (default for pages), this function can render the date.
--
--   > renderDate "date" "%B %e, %Y" "Date unknown"
--
--   Will render something like @January 32, 2010@.
renderDate :: String -- ^ Key in which the rendered date should be placed.
           -> String -- ^ Format to use on the date.
           -> String -- ^ Default value when the date cannot be parsed.
           -> ContextManipulation
renderDate key format defaultValue context = M.insert key value context
  where
    value = fromMaybe defaultValue pretty
    pretty = do
        filePath <- M.lookup "path" context
        let dateString = substituteRegex "^([0-9]*-[0-9]*-[0-9]*).*" "\\1"
                                         (takeFileName filePath)
        time <- parseTime defaultTimeLocale
                          "%Y-%m-%d"
                          dateString :: Maybe UTCTime
        return $ formatTime defaultTimeLocale format time

-- | Change the extension of a file. This is only needed when you want to
--   render, for example, mardown to @.php@ files instead of @.html@ files.
--
--   > renderChainWith (changeExtension "php")
--   >                 ["templates/default.html"]
--   >                 (createPagePath "test.markdown")
--
--   Will render to @test.php@ instead of @test.html@.
changeExtension :: String -- ^ Extension to change to.
                   -> ContextManipulation
changeExtension extension = changeValue "url" changeExtension'
  where
    changeExtension' = flip addExtension extension . dropExtension
