-- | This module exports a number of functions that produce @HakyllAction@s to
--   manipulate @Context@s.
module Text.Hakyll.ContextManipulations
    ( renderValue
    , changeValue
    , changeUrl
    , copyValue
    , renderDate
    , changeExtension
    , renderBody
    ) where

import Control.Monad (liftM)
import Control.Arrow (arr)
import System.Locale (defaultTimeLocale)
import System.FilePath (takeFileName, addExtension, dropExtension)
import Data.Time.Format (parseTime, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Text.Hakyll.Regex (substituteRegex)
import Text.Hakyll.HakyllAction (HakyllAction (..))
import Text.Hakyll.Context (Context)

-- | Do something with a value in a @Context@, but keep the old value as well.
--   If the key given is not present in the @Context@, nothing will happen.
renderValue :: String             -- ^ Key of which the value should be copied.
            -> String             -- ^ Key the value should be copied to.
            -> (String -> String) -- ^ Function to apply on the value.
            -> HakyllAction Context Context
renderValue source destination f = arr $ \context ->
    case M.lookup source context of
        Nothing      -> context
        (Just value) -> M.insert destination (f value) context

-- | Change a value in a @Context@.
--
--   > import Data.Char (toUpper)
--   > changeValue "title" (map toUpper)
--
--   Will put the title in UPPERCASE.
changeValue :: String             -- ^ Key to change.
            -> (String -> String) -- ^ Function to apply on the value.
            -> HakyllAction Context Context
changeValue key = renderValue key key

-- | Change the URL of a page. This requires a special function, so dependency
-- handling can happen correctly.
-- 
changeUrl :: (String -> String)            -- ^ Function to change URL with.
          -> HakyllAction Context Context  -- ^ Resulting action.
changeUrl f = let action = changeValue "url" f
              in action {actionUrl = Right $ liftM f}

-- | Copy a value from one key to another in a @Context@.
copyValue :: String -- ^ Source key.
          -> String -- ^ Destination key.
          -> HakyllAction Context Context
copyValue source destination = renderValue source destination id

-- | When the context has a key called @path@ in a
--   @folder/yyyy-mm-dd-title.extension@ format (the convention for pages),
--   this function can render the date.
--
--   > renderDate "date" "%B %e, %Y" "Date unknown"
--
--   Will render something like @January 32, 2010@.
renderDate :: String -- ^ Key in which the rendered date should be placed.
           -> String -- ^ Format to use on the date.
           -> String -- ^ Default key, in case the date cannot be parsed.
           -> HakyllAction Context Context
renderDate key format defaultValue = renderValue "path" key renderDate'
  where
    renderDate' filePath = fromMaybe defaultValue $ do
        let dateString = substituteRegex "^([0-9]*-[0-9]*-[0-9]*).*" "\\1"
                                         (takeFileName filePath)
        time <- parseTime defaultTimeLocale
                          "%Y-%m-%d"
                          dateString :: Maybe UTCTime
        return $ formatTime defaultTimeLocale format time

-- | Change the extension of a file. This is only needed when you want to
--   render, for example, mardown to @.php@ files instead of @.html@ files.
--
--   > changeExtension "php"
--
--   Will render @test.markdown@ to @test.php@ instead of @test.html@.
changeExtension :: String -- ^ Extension to change to.
                -> HakyllAction Context Context
changeExtension extension = changeValue "url" changeExtension'
  where
    changeExtension' = flip addExtension extension . dropExtension

-- | Change the body of a file using a certain manipulation.
--
--   > import Data.Char (toUpper)
--   > renderBody (map toUpper)
--
--   Will put the entire body of the page in UPPERCASE.
renderBody :: (String -> String)
           -> HakyllAction Context Context
renderBody = renderValue "body" "body"
