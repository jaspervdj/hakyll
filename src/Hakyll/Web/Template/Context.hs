--------------------------------------------------------------------------------
module Hakyll.Web.Template.Context
    ( Context (..)
    , mapContext
    , field

    , defaultContext
    , bodyField
    , urlField
    , pathField
    , categoryField
    , titleField
    , dateField
    , dateFieldWith
    , modificationTimeField
    , modificationTimeFieldWith
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           (Alternative (..), (<$>))
import           Control.Monad                 (msum)
import           Data.List                     (intercalate)
import qualified Data.Map                      as M
import           Data.Monoid                   (Monoid (..))
import           Data.Time.Clock               (UTCTime (..))
import           Data.Time.Format              (formatTime, parseTime)
import           System.FilePath               (takeBaseName, takeDirectory,
                                                takeFileName)
import           System.Locale                 (TimeLocale, defaultTimeLocale)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.ResourceProvider
import           Hakyll.Core.Util.String       (splitAll)
import           Hakyll.Web.Page.Internal
import           Hakyll.Web.Urls


--------------------------------------------------------------------------------
newtype Context a = Context
    { unContext :: String -> Identifier -> a -> Compiler String
    }


--------------------------------------------------------------------------------
instance Monoid (Context a) where
    mempty                          = Context $ \_ _ _ -> empty
    mappend (Context f) (Context g) = Context $ \k i x -> f k i x <|> g k i x


--------------------------------------------------------------------------------
mapContext :: (String -> String) -> Context a -> Context a
mapContext f (Context g) = Context $ \k i x -> f <$> g k i x


--------------------------------------------------------------------------------
field :: String -> (Identifier -> a -> Compiler String) -> Context a
field key value = Context $ \k i x -> if k == key then value i x else empty


--------------------------------------------------------------------------------
defaultContext :: Context Page
defaultContext =
    bodyField     "body"     `mappend`
    urlField      "url"      `mappend`
    pathField     "path"     `mappend`
    categoryField "category" `mappend`
    titleField    "title"    `mappend`
    missingField


--------------------------------------------------------------------------------
bodyField :: String -> Context Page
bodyField key = field key $ \_ x -> return x


--------------------------------------------------------------------------------
urlField :: String -> Context a
urlField key = field key $ \i _ -> maybe empty toUrl <$> getRouteFor i


--------------------------------------------------------------------------------
pathField :: String -> Context a
pathField key = field key $ \i _ -> return $ toFilePath i


--------------------------------------------------------------------------------
categoryField :: String -> Context a
categoryField key = mapContext (takeBaseName . takeDirectory) $ pathField key


--------------------------------------------------------------------------------
titleField :: String -> Context a
titleField key = mapContext takeBaseName $ pathField key


--------------------------------------------------------------------------------
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
dateField :: String     -- ^ Key in which the rendered date should be placed
          -> String     -- ^ Format to use on the date
          -> Context a  -- ^ Resulting context
dateField = dateFieldWith defaultTimeLocale


--------------------------------------------------------------------------------
-- | This is an extended version of 'dateField' that allows you to
-- specify a time locale that is used for outputting the date. For more
-- details, see 'dateField'.
dateFieldWith :: TimeLocale  -- ^ Output time locale
              -> String      -- ^ Destination key
              -> String      -- ^ Format to use on the date
              -> Context a   -- ^ Resulting context
dateFieldWith locale key format = field key $ \id' _ -> do
    time <- getUTC locale id'
    return $ formatTime locale format time


--------------------------------------------------------------------------------
-- | Parser to try to extract and parse the time from the @published@
-- field or from the filename. See 'renderDateField' for more information.
getUTC :: TimeLocale        -- ^ Output time locale
       -> Identifier        -- ^ Input page
       -> Compiler UTCTime  -- ^ Parsed UTCTime
getUTC locale id' = do
    metadata <- getMetadataFor id'
    let tryField k fmt = M.lookup k metadata >>= parseTime' fmt
        fn             = takeFileName $ toFilePath id'

    maybe empty return $ msum $
        [tryField "published" fmt | fmt <- formats] ++
        [tryField "date"      fmt | fmt <- formats] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn]
  where
    parseTime' = parseTime locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S UT"
        , "%Y-%m-%dT%H:%M:%SZ"
        , "%Y-%m-%d %H:%M:%S"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        ]


--------------------------------------------------------------------------------
modificationTimeField :: String     -- ^ Key
                      -> String     -- ^ Format
                      -> Context  a -- ^ Resuting context
modificationTimeField = modificationTimeFieldWith defaultTimeLocale


--------------------------------------------------------------------------------
modificationTimeFieldWith :: TimeLocale  -- ^ Time output locale
                          -> String      -- ^ Key
                          -> String      -- ^ Format
                          -> Context a   -- ^ Resulting context
modificationTimeFieldWith locale key fmt = field key $ \id' _ -> do
    mtime <- compilerUnsafeIO $ resourceModificationTime id'
    return $ formatTime locale fmt mtime


--------------------------------------------------------------------------------
missingField :: Context a
missingField = Context $ \k _ _ -> return $ "$" ++ k ++ "$"
