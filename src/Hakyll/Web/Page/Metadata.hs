-- | Provides various functions to manipulate the metadata fields of a page
--
module Hakyll.Web.Page.Metadata
    ( getField
    , getFieldMaybe
    , setField
    , trySetField
    , setFieldA
    , setFieldPage
    , renderField
    , changeField
    , copyField
    , renderDateField
    , renderDateFieldWith
    , renderModificationTime
    , renderModificationTimeWith
    , copyBodyToField
    , copyBodyFromField
    , comparePagesByDate
    ) where

import Control.Arrow (Arrow, arr, (>>>), (***), (&&&))
import Control.Category (id)
import Control.Monad (msum)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Prelude hiding (id)
import System.FilePath (takeFileName)
import System.Locale (TimeLocale, defaultTimeLocale)
import qualified Data.Map as M

import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (parseTime, formatTime)

import Hakyll.Web.Page.Internal
import Hakyll.Core.Util.String
import Hakyll.Core.Identifier
import Hakyll.Core.Compiler
import Hakyll.Core.Resource.Provider

-- | Get a metadata field. If the field does not exist, the empty string is
-- returned.
--
getField :: String  -- ^ Key
         -> Page a  -- ^ Page
         -> String  -- ^ Value
getField key = fromMaybe "" . getFieldMaybe key

-- | Get a field in a 'Maybe' wrapper
--
getFieldMaybe :: String        -- ^ Key
              -> Page a        -- ^ Page
              -> Maybe String  -- ^ Value, if found
getFieldMaybe key = M.lookup key . pageMetadata

-- | Version of 'trySetField' which overrides any previous value
--
setField :: String  -- ^ Key
         -> String  -- ^ Value
         -> Page a  -- ^ Page to add it to
         -> Page a  -- ^ Resulting page
setField k v (Page m b) = Page (M.insert k v m) b

-- | Add a metadata field. If the field already exists, it is not overwritten.
--
trySetField :: String  -- ^ Key
            -> String  -- ^ Value
            -> Page a  -- ^ Page to add it to
            -> Page a  -- ^ Resulting page
trySetField k v (Page m b) = Page (M.insertWith (flip const) k v m) b

-- | Arrow-based variant of 'setField'. Because of it's type, this function is
-- very usable together with the different 'require' functions.
--
setFieldA :: Arrow a
          => String                  -- ^ Key
          -> a x String              -- ^ Value arrow
          -> a (Page b, x) (Page b)  -- ^ Resulting arrow
setFieldA k v = id *** v >>> arr (uncurry $ flip $ setField k)

-- | Set a field of a page to the contents of another page
--
setFieldPage :: String                      -- ^ Key to add the page under
             -> Identifier (Page String)    -- ^ Page to add
             -> Compiler (Page a) (Page a)  -- ^ Page compiler
setFieldPage key page = id &&& require_ page >>> setFieldA key (arr pageBody)

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

-- | When the metadata has a field called @published@ in one of the
-- following formats then this function can render the date.
--
--   * @Sun, 01 Feb 2000 13:00:00 UT@ (RSS date format)
--
--   * @2000-02-01T13:00:00Z@ (Atom date format)
--
--   * @February 1, 2000 1:00 PM@ (PM is usually uppercase)
--
--   * @February 1, 2000@ (assumes 12:00 AM for the time)
--
-- Alternatively, when the metadata has a field called @path@ in a
-- @folder/yyyy-mm-dd-title.extension@ format (the convention for pages)
-- and no @published@ metadata field set, this function can render
-- the date.
--
-- > renderDateField "date" "%B %e, %Y" "Date unknown"
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
renderDateFieldWith locale key format defaultValue page =
    setField key renderTimeString page
  where
    renderTimeString = fromMaybe defaultValue $ do
        time <- getUTCMaybe locale page
        return $ formatTime locale format time

-- | Parser to try to extract and parse the time from the @published@
-- field or from the filename. See 'renderDateField' for more information.
getUTCMaybe :: TimeLocale     -- ^ Output time locale
            -> Page a         -- ^ Input page
            -> Maybe UTCTime  -- ^ Parsed UTCTime
getUTCMaybe locale page = msum
    [ fromPublished "%a, %d %b %Y %H:%M:%S UT"
    , fromPublished "%Y-%m-%dT%H:%M:%SZ"
    , fromPublished "%B %e, %Y %l:%M %p"
    , fromPublished "%B %e, %Y"
    , getFieldMaybe "path" page >>= parseTime' "%Y-%m-%d" .
        intercalate "-" . take 3 . splitAll "-" . takeFileName
    ]
  where
    fromPublished f  = getFieldMaybe "published" page >>= parseTime' f
    parseTime' f str = parseTime locale f str

-- | Set the modification time as a field in the page
renderModificationTime :: String
                       -- ^ Destination key
                       -> String
                       -- ^ Format to use on the time
                       -> Compiler (Page String) (Page String)
                       -- ^ Resulting compiler
renderModificationTime = renderModificationTimeWith defaultTimeLocale

renderModificationTimeWith :: TimeLocale
                           -- ^ Output time locale
                           -> String
                           -- ^ Destination key
                           -> String
                           -- ^ Format to use on the time
                           -> Compiler (Page String) (Page String)
                           -- ^ Resulting compiler
renderModificationTimeWith locale key format =
    id &&& (getResource >>> getResourceWith resourceModificationTime) >>>
    setFieldA key (arr (formatTime locale format))

-- | Copy the body of a page to a metadata field
--
copyBodyToField :: String       -- ^ Destination key
                -> Page String  -- ^ Target page
                -> Page String  -- ^ Resulting page
copyBodyToField key page = setField key (pageBody page) page

-- | Copy a metadata field to the page body
--
copyBodyFromField :: String       -- ^ Source key
                  -> Page String  -- ^ Target page
                  -> Page String  -- ^ Resulting page
copyBodyFromField key page = fmap (const $ getField key page) page

-- | Compare pages by the date and time parsed as in 'renderDateField',
-- where 'LT' implies earlier, and 'GT' implies later. For more details,
-- see 'renderDateField'.
comparePagesByDate :: Page a -> Page a -> Ordering
comparePagesByDate = comparing $ fromMaybe zero . getUTCMaybe defaultTimeLocale
  where
    zero = UTCTime (ModifiedJulianDay 0) 0
