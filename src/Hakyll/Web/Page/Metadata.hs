-- | Provides various functions to manipulate the metadata fields of a page
--
module Hakyll.Web.Page.Metadata
    ( getField
    , setField
    , setFieldA
    , renderField
    , changeField
    , copyField
    , renderDateField
    , renderDateFieldWith
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow (Arrow, (>>>), (***), arr)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import qualified Data.Map as M
import System.FilePath (takeFileName)
import System.Locale (TimeLocale, defaultTimeLocale)

import Hakyll.Web.Page.Internal
import Hakyll.Web.Util.String

-- | Get a metadata field. If the field does not exist, the empty string is
-- returned.
--
getField :: String  -- ^ Key
         -> Page a  -- ^ Page
         -> String  -- ^ Value
getField key = fromMaybe "" . M.lookup key . pageMetadata

-- | Add a metadata field. If the field already exists, it is not overwritten.
--
setField :: String  -- ^ Key
         -> String  -- ^ Value
         -> Page a  -- ^ Page to add it to
         -> Page a  -- ^ Resulting page
setField k v (Page m b) = Page (M.insertWith (flip const) k v m) b

-- | Arrow-based variant of 'setField'. Because of it's type, this function is
-- very usable together with the different 'require' functions.
--
setFieldA :: Arrow a
          => String                  -- ^ Key
          -> a x String              -- ^ Value arrow
          -> a (Page b, x) (Page b)  -- ^ Resulting arrow
setFieldA k v = id *** v >>> arr (uncurry $ flip $ setField k)

-- | Do something with a metadata value, but keep the old value as well. If the
-- key given is not present in the metadata, nothing will happen. If the source
-- and destination keys are the same, the value will be changed (but you should
-- use 'changeField' for this purpose).
--
renderField :: String              -- ^ Key of which the value should be copied
            -> String              -- ^ Key the value should be copied to
            -> (String -> String)  -- ^ Function to apply on the value
            -> Page a              -- ^ Page on which this should be applied
            -> Page a              -- ^ Resulting page
renderField src dst f page = case M.lookup src (pageMetadata page) of
    Nothing    -> page
    Just value -> setField dst (f value) page

-- | Change a metadata value.
--
-- > import Data.Char (toUpper)
-- > changeField "title" (map toUpper)
--
-- Will put the title in UPPERCASE.
--
changeField :: String              -- ^ Key to change.
            -> (String -> String)  -- ^ Function to apply on the value.
            -> Page a              -- ^ Page to change
            -> Page a              -- ^ Resulting page
changeField key = renderField key key

-- | Make a copy of a metadata field (put the value belonging to a certain key
-- under some other key as well)
--
copyField :: String  -- ^ Key to copy
          -> String  -- ^ Destination to copy to
          -> Page a  -- ^ Page on which this should be applied
          -> Page a  -- ^ Resulting page
copyField src dst = renderField src dst id

-- | When the metadata has a field called @path@ in a
-- @folder/yyyy-mm-dd-title.extension@ format (the convention for pages),
-- this function can render the date.
--
-- > renderDate "date" "%B %e, %Y" "Date unknown"
--
-- Will render something like @January 32, 2010@.
--
renderDateField :: String  -- ^ Key in which the rendered date should be placed
                -> String  -- ^ Format to use on the date
                -> String  -- ^ Default value, in case the date cannot be parsed
                -> Page a  -- ^ Page on which this should be applied
                -> Page a  -- ^ Resulting page
renderDateField = renderDateFieldWith defaultTimeLocale

-- | This is an extended version of 'renderDateField' that allows you to
-- specify a time locale that is used for outputting the date. For more
-- details, see 'renderDateField'.
--
renderDateFieldWith :: TimeLocale  -- ^ Output time locale
                    -> String      -- ^ Destination key
                    -> String      -- ^ Format to use on the date
                    -> String      -- ^ Default value
                    -> Page a      -- ^ Target page
                    -> Page a      -- ^ Resulting page
renderDateFieldWith locale key format defaultValue =
    renderField "path" key renderDate'
  where
    renderDate' filePath = fromMaybe defaultValue $ do
        let dateString = intercalate "-" $ take 3
                       $ splitAll "-" $ takeFileName filePath
        time <- parseTime defaultTimeLocale
                          "%Y-%m-%d"
                          dateString :: Maybe UTCTime
        return $ formatTime locale format time
