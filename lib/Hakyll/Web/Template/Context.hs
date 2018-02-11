--------------------------------------------------------------------------------
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
module Hakyll.Web.Template.Context
    ( ContextField (..)
    , Context (..)
    , field
    , boolField
    , constField
    , listField
    , listFieldWith
    , functionField
    , mapContext

    , defaultContext
    , bodyField
    , metadataField
    , urlField
    , pathField
    , titleField
    , snippetField
    , dateField
    , dateFieldWith
    , getItemUTC
    , getItemModificationTime
    , modificationTimeField
    , modificationTimeFieldWith
    , teaserField
    , teaserFieldWithSeparator
    , missingField
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           (Alternative (..))
import           Control.Monad                 (msum)
import           Data.List                     (intercalate)
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup                (Semigroup (..))
#endif
import           Data.Time.Clock               (UTCTime (..))
import           Data.Time.Format              (formatTime)
import qualified Data.Time.Format              as TF
import           Data.Time.Locale.Compat       (TimeLocale, defaultTimeLocale)
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider
import           Hakyll.Core.Util.String       (needlePrefix, splitAll)
import           Hakyll.Web.Html
import           System.FilePath               (splitDirectories, takeBaseName)


--------------------------------------------------------------------------------
-- | Mostly for internal usage
data ContextField
    = StringField String
    | forall a. ListField (Context a) [Item a]


--------------------------------------------------------------------------------
-- | The 'Context' monoid. Please note that the order in which you
-- compose the items is important. For example in
--
-- > field "A" f1 <> field "A" f2
--
-- the first context will overwrite the second. This is especially
-- important when something is being composed with
-- 'metadataField' (or 'defaultContext'). If you want your context to be
-- overwritten by the metadata fields, compose it from the right:
--
-- @
-- 'metadataField' \<\> field \"date\" fDate
-- @
--
newtype Context a = Context
    { unContext :: String -> [String] -> Item a -> Compiler ContextField
    }


--------------------------------------------------------------------------------
#if MIN_VERSION_base(4,9,0)
instance Semigroup (Context a) where
    (<>) (Context f) (Context g) = Context $ \k a i -> f k a i <|> g k a i

instance Monoid (Context a) where
    mempty  = missingField
    mappend = (<>)
#else
instance Monoid (Context a) where
    mempty                          = missingField
    mappend (Context f) (Context g) = Context $ \k a i -> f k a i <|> g k a i
#endif


--------------------------------------------------------------------------------
field' :: String -> (Item a -> Compiler ContextField) -> Context a
field' key value = Context $ \k _ i -> if k == key then value i else empty


--------------------------------------------------------------------------------
-- | Constructs a new field in the 'Context.'
field
    :: String                      -- ^ Key
    -> (Item a -> Compiler String) -- ^ Function that constructs a value based
                                   -- on the item
    -> Context a
field key value = field' key (fmap StringField . value)


--------------------------------------------------------------------------------
-- | Creates a 'field' to use with the @$if()$@ template macro.
boolField
    :: String
    -> (Item a -> Bool)
    -> Context a
boolField name f = field name (\i -> if f i
    then pure (error $ unwords ["no string value for bool field:",name])
    else empty)


--------------------------------------------------------------------------------
-- | Creates a 'field' that does not depend on the 'Item'
constField :: String -> String -> Context a
constField key = field key . const . return


--------------------------------------------------------------------------------
listField :: String -> Context a -> Compiler [Item a] -> Context b
listField key c xs = listFieldWith key c (const xs)


--------------------------------------------------------------------------------
listFieldWith
    :: String -> Context a -> (Item b -> Compiler [Item a]) -> Context b
listFieldWith key c f = field' key $ fmap (ListField c) . f


--------------------------------------------------------------------------------
functionField :: String -> ([String] -> Item a -> Compiler String) -> Context a
functionField name value = Context $ \k args i ->
    if k == name
        then StringField <$> value args i
        else empty


--------------------------------------------------------------------------------
mapContext :: (String -> String) -> Context a -> Context a
mapContext f (Context c) = Context $ \k a i -> do
    fld <- c k a i
    case fld of
        StringField str -> return $ StringField (f str)
        ListField _ _   -> fail $
            "Hakyll.Web.Template.Context.mapContext: " ++
            "can't map over a ListField!"

--------------------------------------------------------------------------------
-- | A context that allows snippet inclusion. In processed file, use as:
--
-- > ...
-- > $snippet("path/to/snippet/")$
-- > ...
--
-- The contents of the included file will not be interpolated.
--
snippetField :: Context String
snippetField = functionField "snippet" f
  where
    f [contentsPath] _ = loadBody (fromFilePath contentsPath)
    f _              i = error $
        "Too many arguments to function 'snippet()' in item " ++
            show (itemIdentifier i)

--------------------------------------------------------------------------------
-- | A context that contains (in that order)
--
--     1. A @$body$@ field
--
--     2. Metadata fields
--
--     3. A @$url$@ 'urlField'
--
--     4. A @$path$@ 'pathField'
--
--     5. A @$title$@ 'titleField'
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
-- | Constructs a 'field' that contains the body of the item.
bodyField :: String -> Context String
bodyField key = field key $ return . itemBody


--------------------------------------------------------------------------------
-- | Map any field to its metadata value, if present
metadataField :: Context a
metadataField = Context $ \k _ i -> do
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
-- | This title 'field' takes the basename of the underlying file by default
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
-- the date. This pattern matches the file name or directory names
-- that begins with @yyyy-mm-dd@ . For example:
-- @folder//yyyy-mm-dd-title//dist//main.extension@ .
-- In case of multiple matches, the rightmost one is used.

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
    let tryField k fmt = lookupString k metadata >>= parseTime' fmt
        paths          = splitDirectories $ toFilePath id'

    maybe empty' return $ msum $
        [tryField "published" fmt | fmt <- formats] ++
        [tryField "date"      fmt | fmt <- formats] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fnCand | fnCand <- reverse paths]
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC: " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTimeM True locale
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
-- | Get the time on which the actual file was last modified. This only works if
-- there actually is an underlying file, of couse.
getItemModificationTime
    :: Identifier
    -> Compiler UTCTime
getItemModificationTime identifier = do
    provider <- compilerProvider <$> compilerAsk
    return $ resourceModificationTime provider identifier


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
    mtime <- getItemModificationTime $ itemIdentifier i
    return $ formatTime locale fmt mtime


--------------------------------------------------------------------------------
-- | A context with "teaser" key which contain a teaser of the item.
-- The item is loaded from the given snapshot (which should be saved
-- in the user code before any templates are applied).
teaserField :: String           -- ^ Key to use
            -> Snapshot         -- ^ Snapshot to load
            -> Context String   -- ^ Resulting context
teaserField = teaserFieldWithSeparator teaserSeparator


--------------------------------------------------------------------------------
-- | A context with "teaser" key which contain a teaser of the item, defined as
-- the snapshot content before the teaser separator. The item is loaded from the
-- given snapshot (which should be saved in the user code before any templates
-- are applied).
teaserFieldWithSeparator :: String           -- ^ Separator to use
                         -> String           -- ^ Key to use
                         -> Snapshot         -- ^ Snapshot to load
                         -> Context String   -- ^ Resulting context
teaserFieldWithSeparator separator key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    case needlePrefix separator body of
        Nothing -> fail $
            "Hakyll.Web.Template.Context: no teaser defined for " ++
            show (itemIdentifier item)
        Just t -> return t


--------------------------------------------------------------------------------
missingField :: Context a
missingField = Context $ \k _ i -> fail $
    "Missing field $" ++ k ++ "$ in context for item " ++
    show (itemIdentifier i)

parseTimeM :: Bool -> TimeLocale -> String -> String -> Maybe UTCTime
#if MIN_VERSION_time(1,5,0)
parseTimeM = TF.parseTimeM
#else
parseTimeM _ = TF.parseTime
#endif
