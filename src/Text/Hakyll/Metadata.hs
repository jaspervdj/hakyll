-- | This module exports a number of functions to manipulate metadata of
-- resources
--
module Text.Hakyll.ContextManipulations
    ( renderValue
    , changeValue
    , changeUrl
    , copyValue
    , renderDate
    , renderDateWithLocale
    , changeExtension
    ) where

import System.Locale (TimeLocale, defaultTimeLocale)
import System.FilePath (takeFileName, addExtension, dropExtension)
import Data.Time.Format (parseTime, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Text.Hakyll.Regex (substituteRegex)
import Text.Hakyll.Transformer (Transformer (..), transformMetadata)
import Text.Hakyll.Resource

-- | Do something with a value in a @Context@, but keep the old value as well.
--   If the key given is not present in the @Context@, nothing will happen.
--
renderValue :: String              -- ^ Key of which the value should be copied.
            -> String              -- ^ Key the value should be copied to.
            -> (String -> String)  -- ^ Function to apply on the value.
            -> Transformer a a     -- ^ Resulting transformer
renderValue source destination f = transformMetadata $ \(Metadata m) ->
    Metadata $ case M.lookup source m of
        Nothing      -> m
        (Just value) -> M.insert destination (f value) m

-- | Change a value in the metadata
--
--   > import Data.Char (toUpper)
--   > changeValue "title" (map toUpper)
--
--   Will put the title in UPPERCASE.
changeValue :: String              -- ^ Key to change.
            -> (String -> String)  -- ^ Function to apply on the value.
            -> Transformer a a
changeValue key = renderValue key key

-- | Change the URL of a page. You should always use this function instead of
-- 'changeValue' for this, because using 'changeValue' might break dependency
-- handling when changing the @url@ field.
-- 
changeUrl :: (String -> String)  -- ^ Function to change URL with.
          -> Transformer a a     -- ^ Resulting action.
changeUrl f = let t = changeValue "url" f
              in t {transformerUrl = return . f}

-- | Copy a metadata value from one key to another
--
copyValue :: String           -- ^ Source key.
          -> String           -- ^ Destination key.
          -> Transformer a a  -- ^ Resulting transformer
copyValue source destination = renderValue source destination id

-- | When the context has a key called @path@ in a
--   @folder/yyyy-mm-dd-title.extension@ format (the convention for pages),
--   this function can render the date.
--
--   > renderDate "date" "%B %e, %Y" "Date unknown"
--
--   Will render something like @January 32, 2010@.
--
renderDate :: String -- ^ Key in which the rendered date should be placed.
           -> String -- ^ Format to use on the date.
           -> String -- ^ Default key, in case the date cannot be parsed.
           -> Transformer a a
renderDate = renderDateWithLocale defaultTimeLocale

-- | This is an extended version of 'renderDate' that allows you to specify a
-- time locale that is used for outputting the date. For more details, see
-- 'renderDate'.
--
renderDateWithLocale :: TimeLocale  -- ^ Output time locale.
                     -> String      -- ^ Destination key.
                     -> String      -- ^ Format to use on the date.
                     -> String      -- ^ Default key.
                     -> Transformer a a
renderDateWithLocale locale key format defaultValue =
    renderValue "path" key renderDate'
  where
    renderDate' filePath = fromMaybe defaultValue $ do
        let dateString = substituteRegex "^([0-9]*-[0-9]*-[0-9]*).*" "\\1"
                                         (takeFileName filePath)
        time <- parseTime defaultTimeLocale
                          "%Y-%m-%d"
                          dateString :: Maybe UTCTime
        return $ formatTime locale format time

-- | Change the extension of a file. This is only needed when you want to
--   render, for example, mardown to @.php@ files instead of @.html@ files.
--
--   > changeExtension "php"
--
--   Will render @test.markdown@ to @test.php@ instead of @test.html@.
changeExtension :: String           -- ^ Extension to change to.
                -> Transformer a a  -- ^ Resulting transformer
changeExtension extension = changeValue "url" changeExtension'
  where
    changeExtension' = flip addExtension extension . dropExtension
