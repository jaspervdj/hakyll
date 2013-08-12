--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
module Hakyll.Web.Template.Context
    ( ContextField (..)
    , Context (..)
    , field
    , constField
    , listField
    , functionField
    , mapContext

    , defaultContext
    , bodyField
    , metadataField
    , urlField
    , pathField
    , titleField
    , dateField
    , dateFieldWith
    , getItemUTC
    , modificationTimeField
    , modificationTimeFieldWith
    , teaserField
    , missingField
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           (Alternative (..), (<$>))
import           Control.Monad                 (msum)
import           Data.List                     (intercalate)
import qualified Data.Map                      as M
import           Data.Monoid                   (Monoid (..))
import           Data.Time.Clock               (UTCTime (..))
import           Data.Time.Format              (formatTime, parseTime)
import           System.FilePath               (takeBaseName, takeFileName)
import           System.Locale                 (TimeLocale, defaultTimeLocale)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider
import           Hakyll.Core.Util.String       (splitAll, needlePrefix)
import           Hakyll.Web.Html


--------------------------------------------------------------------------------
-- | Mostly for internal usage
data ContextField
    = StringField String
    | forall a. ListField (Context a) [Item a]


--------------------------------------------------------------------------------
newtype Context a = Context
    { unContext :: String -> Item a -> Compiler ContextField
    }


--------------------------------------------------------------------------------
instance Monoid (Context a) where
    mempty                          = missingField
    mappend (Context f) (Context g) = Context $ \k i -> f k i <|> g k i


--------------------------------------------------------------------------------
field' :: String -> (Item a -> Compiler ContextField) -> Context a
field' key value = Context $ \k i -> if k == key then value i else empty


--------------------------------------------------------------------------------
field :: String -> (Item a -> Compiler String) -> Context a
field key value = field' key (fmap StringField . value)


--------------------------------------------------------------------------------
constField :: String -> String -> Context a
constField key = field key . const . return


--------------------------------------------------------------------------------
listField :: String -> Context a -> Compiler [Item a] -> Context b
listField key c xs = field' key $ \_ -> fmap (ListField c) xs


--------------------------------------------------------------------------------
functionField :: String -> ([String] -> Item a -> Compiler String) -> Context a
functionField name value = Context $ \k i -> case words k of
    []              -> empty
    (n : args)
        | n == name -> StringField <$> value args i
        | otherwise -> empty


--------------------------------------------------------------------------------
mapContext :: (String -> String) -> Context a -> Context a
mapContext f (Context c) = Context $ \k i -> do
    fld <- c k i
    case fld of
        StringField str -> return $ StringField (f str)
        ListField _ _   -> fail $
            "Hakyll.Web.Template.Context.mapContext: " ++
            "can't map over a ListField!"


--------------------------------------------------------------------------------
defaultContext :: Context String
defaultContext =
    bodyField     "body"     `mappend`
    metadataField            `mappend`
    urlField      "url"      `mappend`
    pathField     "path"     `mappend`
    titleField    "title"    `mappend`
    missingField


--------------------------------------------------------------------------------
teaserSeparator :: String
teaserSeparator = "<!--more-->"


--------------------------------------------------------------------------------
bodyField :: String -> Context String
bodyField key = field key $ return . itemBody


--------------------------------------------------------------------------------
-- | Map any field to its metadata value, if present
metadataField :: Context String
metadataField = Context $ \k i -> do
    value <- getMetadataField (itemIdentifier i) k
    maybe empty (return . StringField) value


--------------------------------------------------------------------------------
-- | Absolute url to the resulting item
urlField :: String -> Context a
urlField key = field key $
    fmap (maybe empty toUrl) . getRoute . itemIdentifier


--------------------------------------------------------------------------------
-- | Filepath of the underlying file of the item
pathField :: String -> Context a
pathField key = field key $ return . toFilePath . itemIdentifier


--------------------------------------------------------------------------------
-- | This title field takes the basename of the underlying file by default
titleField :: String -> Context a
titleField = mapContext takeBaseName . pathField


--------------------------------------------------------------------------------
-- | When the metadata has a field called @published@ in one of the
-- following formats then this function can render the date.
--
--   * @Mon, 06 Sep 2010 00:01:00 +0000@
--
--   * @Mon, 06 Sep 2010 00:01:00 UTC@
--
--   * @Mon, 06 Sep 2010 00:01:00@
--
--   * @2010-09-06T00:01:00+0000@
--
--   * @2010-09-06T00:01:00Z@
--
--   * @2010-09-06T00:01:00@
--
--   * @2010-09-06 00:01:00+0000@
--
--   * @2010-09-06 00:01:00@
--
--   * @September 06, 2010 00:01 AM@
--
-- Following date-only formats are supported too (@00:00:00@ for time is
-- assumed)
--
--   * @2010-09-06@
--
--   * @September 06, 2010@
--
-- Alternatively, when the metadata has a field called @path@ in a
-- @folder/yyyy-mm-dd-title.extension@ format (the convention for pages)
-- and no @published@ metadata field set, this function can render
-- the date.
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
dateFieldWith locale key format = field key $ \i -> do
    time <- getItemUTC locale $ itemIdentifier i
    return $ formatTime locale format time


--------------------------------------------------------------------------------
-- | Parser to try to extract and parse the time from the @published@
-- field or from the filename. See 'dateField' for more information.
-- Exported for user convenience.
getItemUTC :: MonadMetadata m
           => TimeLocale        -- ^ Output time locale
           -> Identifier        -- ^ Input page
           -> m UTCTime         -- ^ Parsed UTCTime
getItemUTC locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = M.lookup k metadata >>= parseTime' fmt
        fn             = takeFileName $ toFilePath id'

    maybe empty' return $ msum $
        [tryField "published" fmt | fmt <- formats] ++
        [tryField "date"      fmt | fmt <- formats] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn]
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC: " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTime locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
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
modificationTimeFieldWith locale key fmt = field key $ \i -> do
    provider <- compilerProvider <$> compilerAsk
    let mtime = resourceModificationTime provider $ itemIdentifier i
    return $ formatTime locale fmt mtime


--------------------------------------------------------------------------------
-- | A context with "teaser" key which contain a teaser of the item.
-- The item is loaded from the given snapshot (which should be saved
-- in the user code before any templates are applied).
teaserField :: String           -- ^ Key to use
            -> Snapshot         -- ^ Snapshot to load
            -> Context String   -- ^ Resulting context
teaserField key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    case needlePrefix teaserSeparator body of
        Nothing -> fail $
            "Hakyll.Web.Template.Context: no teaser defined for " ++
            show (itemIdentifier item)
        Just t -> return t


--------------------------------------------------------------------------------
missingField :: Context a
missingField = Context $ \k i -> fail $
    "Missing field $" ++ k ++ "$ in context for item " ++
    show (itemIdentifier i)
