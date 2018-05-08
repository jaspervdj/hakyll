-- | This module provides 'Context's which are used to expand expressions in
-- templates and allow for arbitrary customisation.
--
-- 'Template's define a small expression DSL which consists of strings,
-- identifiers and function application. There is no type system, every value is
-- a string and on the top level they get substituted verbatim into the page.
--
-- For example, you can build a context that contains
--
-- > … <> functionField "concat" (const . concat) <> …
--
-- which will allow you to use the @concat@ identifier as a function that takes
-- arbitrarily many stings and concatenates them to a new string:
--
-- > $partial(concat("templates/categories/", category))$
--
-- This will evaluate the @category@ field in the context, then prepend he path,
-- and include the referenced file as a template.


--------------------------------------------------------------------------------
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Hakyll.Web.Template.Context
    ( ContextField (..)
    , Context (..)
    , context
    , functionContext
    , toContextField
    , field
    , boolField
    , constField
    , listField
    , listFieldWith
    , functionField
    , dataField
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
import           Data.Char                     (toUpper)
import           Text.Read                     (readMaybe)
import           Data.List                     (intercalate)
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup                (Semigroup (..))
#endif
import           Data.Yaml                     (Value (..))
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as H
import qualified Data.Vector                   as V
import           Data.Yaml.Extended            (toString)
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
import           Prelude                       hiding (id)


--------------------------------------------------------------------------------
-- | Mostly for internal usage
data ContextField
    = EmptyField
    | StringField String
    | forall a. ListField (Context a) [Item a]
    | forall a. LexicalListField (forall b. Context b -> a -> Context b) [a]

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
-- | Tries to find a key in the left context,
-- or when that fails in the right context.
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
class ToContextField a where
    toContextField :: a -> Compiler ContextField

instance ToContextField ContextField where
    toContextField = return

instance ToContextField a => ToContextField (Compiler a) where
    toContextField = (>>= toContextField)

instance ToContextField [Char] where
    toContextField = return . StringField

instance ToContextField Bool where
    toContextField True  = return EmptyField
    toContextField False = failBranch "False"

instance ToContextField a => ToContextField (Maybe a) where
    toContextField = maybe (failBranch "False") toContextField


--------------------------------------------------------------------------------
functionContext :: ToContextField c => String -> ([String] -> Item a -> c) -> Context a
functionContext key value = Context $ \k args item ->
    if k == key
        then mapError details $ toContextField $ value args item
        else failBranch $ "Tried field " ++ key
  where
    details []        = ["No result at field "++key]
    details ["False"] = ["Field "++key++" is False"]
    details errors    = ("In evaluation of field "++key) : errors

context :: ToContextField c => String -> (Item a -> c) -> Context a
context key = functionContext key . const


--------------------------------------------------------------------------------
-- | Constructs a new field for a 'Context'.
-- If the key matches, the compiler is run and its result is substituted in the
-- template.
-- 
-- If the compiler fails, the field will be considered non-existent
-- in an @$if()$@ macro or ultimately break the template application
-- (unless the key is found in another context when using '<>').
-- Use 'empty' or 'failBranch' for intentional failures of fields used in
-- @$if()$@, to distinguish them from exceptions thrown with 'fail'.
field
    :: String                      -- ^ Key
    -> (Item a -> Compiler String) -- ^ Function that constructs a value based
                                   -- on the item (e.g. accessing metadata)
    -> Context a
field = context


--------------------------------------------------------------------------------
-- | Creates a 'field' to use with the @$if()$@ template macro.
-- Attempting to substitute the field into the template will cause an error.
boolField
    :: String
    -> (Item a -> Bool)
    -> Context a
boolField = context


--------------------------------------------------------------------------------
-- | Creates a 'field' that does not depend on the 'Item' but always yields
-- the same string
constField :: String     -- ^ Key
           -> String     -- ^ Value
           -> Context a
constField key = context key . const


--------------------------------------------------------------------------------
-- | Creates a list field to be consumed by a @$for(…)$@ expression.
-- The compiler returns multiple items which are rendered in the loop body
-- with the supplied context.
listField :: String -> Context a -> Compiler [Item a] -> Context b
listField key c xs = listFieldWith key c (const xs)


--------------------------------------------------------------------------------
-- | Creates a list field like 'listField', but supplies the current page
-- to the compiler.
listFieldWith
    :: String -> Context a -> (Item b -> Compiler [Item a]) -> Context b
listFieldWith key c f = context key $ fmap (ListField c) . f


--------------------------------------------------------------------------------
-- | Creates a variadic function field.
--
-- The function will be called with the dynamically evaluated string arguments
-- from the template as well as the page that is currently rendered.
functionField :: String                                  -- ^ Key
              -> ([String] -> Item a -> Compiler String) -- ^ Function
              -> Context a
functionField = functionContext


--------------------------------------------------------------------------------
-- | Transform the respective string results of all fields in a context.
-- For example,
--
-- > mapContext (++"c") (constField "x" "a" <> constField "y" "b")
--
-- is equivalent to
--
-- > constField "x" "ac" <> constField "y" "bc"
--
mapContext :: (String -> String) -> Context a -> Context a
mapContext f (Context c) = Context $ \k a i -> do
    fld <- c k a i
    case fld of
        EmptyField      -> wrongType "boolField"
        StringField str -> return $ StringField (f str)
        _               -> wrongType "ListField"
  where
    wrongType typ = fail $ "Hakyll.Web.Template.Context.mapContext: " ++
        "can't map over a " ++ typ ++ "!"

--------------------------------------------------------------------------------
-- | A context that allows snippet inclusion. In processed file, use as:
--
-- > ...
-- > $snippet("path/to/snippet/")$
-- > ...
--
-- The contents of the included file will not be interpolated like @partial@
-- does it.
--
snippetField :: Context String
snippetField = functionField "snippet" f
  where
    f [contentsPath] _ = loadBody (fromFilePath contentsPath)
    f []             _ = fail "No argument to function 'snippet()'"
    f _              _ = fail "Too many arguments to function 'snippet()'"



dataField :: String -> Value -> Context a
dataField key val = Context $ \f a _ -> case splitAll "\\." f of
    [k] | k == get    -> lookupNestedValue a val
    (k:ks) | k == key -> lookupNestedValue ks val
    _                 -> failBranch $ "Tried field " ++ key -- and functionField get
  where
    get = let (h:rest) = key in "get" ++ toUpper h : rest

makePairContext :: Context a -> (T.Text, Value) -> Context a
makePairContext c (key, value) = pairContext <> c
  where
    pairContext = Context $ \k a _ -> case splitAll "\\." k of
        ["get"]      -> lookupNestedValue a value
        ["key"]      -> return $ StringField $ T.unpack key
        ("value":ks) -> lookupNestedValue ks value
        []           -> fail "no supposted to happen" -- , right?
        keys         -> lookupNestedValue keys value

makeIndexContext :: Context a -> (Int, Value) -> Context a
makeIndexContext c (index, value) = indexContext <> c
  where
    indexContext = Context $ \k a _ -> case splitAll "\\." k of
        ["get"]      -> lookupNestedValue a value
        ["index"]    -> return $ StringField $ show index
        ("value":ks) -> lookupNestedValue ks value
        []           -> fail "no supposted to happen" -- , right?
        keys         -> lookupNestedValue keys value

lookupNestedValue :: [String] -> Value -> Compiler ContextField
lookupNestedValue []     (Object o) = return $ LexicalListField makePairContext $ H.toList o
lookupNestedValue []     (Array a)  = return $ LexicalListField makeIndexContext $ V.toList $ V.indexed a
lookupNestedValue []     v          = return $ let Just s = toString v in StringField s
lookupNestedValue (k:ks) (Object m) = case H.lookup (T.pack k) m of
    Nothing -> failBranch $ "No '"++k++"' property in object" -- ++ debug m
    Just v  -> lookupNestedValue ks v
lookupNestedValue (k:ks) (Array v)  = case readMaybe k :: Maybe Int of
    Nothing -> failBranch $ "No '"++k++"' element in array" -- ++ debug v
    Just n  -> case v V.!? n of
        Nothing -> failBranch $ "No '"++k++"' index in array of size " ++ show (length v) -- ++ debug v
        Just v  -> lookupNestedValue ks v
lookupNestedValue (k:_)  _          = failBranch $ "no '"++k++"' in primitive value" -- ++ debug p













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
    titleField    "title"


--------------------------------------------------------------------------------
teaserSeparator :: String
teaserSeparator = "<!--more-->"


--------------------------------------------------------------------------------
-- | Constructs a 'field' that contains the body of the item.
bodyField :: String -> Context String
bodyField key = context key itemBody


--------------------------------------------------------------------------------
-- | Map any field to its metadata value, if present
metadataField :: Context a
metadataField = Context $ \k _ i -> do
    let id = itemIdentifier i
        empty' = failBranch $ "No '" ++ k ++ "' field in metadata " ++
            "of item " ++ show id
    value <- getMetadataField id k
    maybe empty' (return . StringField) value


--------------------------------------------------------------------------------
-- | Absolute url to the resulting item
urlField :: String -> Context a
urlField key = field key $ \i -> do
    let id = itemIdentifier i
        empty' = fail $ "No route url found for item " ++ show id
    fmap (maybe empty' toUrl) $ getRoute id


--------------------------------------------------------------------------------
-- | Filepath of the underlying file of the item
pathField :: String -> Context a
pathField key = context key $ toFilePath . itemIdentifier


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
-- details, see 'dateField' and 'formatTime'.
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
-- | Creates a field with the last modification date of the underlying item.
modificationTimeField :: String     -- ^ Key
                      -> String     -- ^ Format
                      -> Context  a -- ^ Resulting context
modificationTimeField = modificationTimeFieldWith defaultTimeLocale


--------------------------------------------------------------------------------
-- | Creates a field with the last modification date of the underlying item
-- in a custom localisation format (see 'formatTime').
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
-- | Constantly reports any field as missing. Mostly for internal usage,
-- it is the last choice in every context used in a template application.
missingField :: Context a
missingField = Context $ \k _ _ -> failBranch $
    "Missing field '" ++ k ++ "' in context"

parseTimeM :: Bool -> TimeLocale -> String -> String -> Maybe UTCTime
#if MIN_VERSION_time(1,5,0)
parseTimeM = TF.parseTimeM
#else
parseTimeM _ = TF.parseTime
#endif
