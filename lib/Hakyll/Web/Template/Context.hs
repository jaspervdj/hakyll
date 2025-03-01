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
-- arbitrarily many strings and concatenates them to a new string:
--
-- > $partial(concat("templates/categories/", category))$
--
-- This will evaluate the @category@ field in the context, then prepend the path,
-- and include the referenced file as a template.


--------------------------------------------------------------------------------
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
module Hakyll.Web.Template.Context
    ( ContextField (..)
    , Context (..)
    , field
    , boolField
    , boolFieldM
    , constField
    , listField
    , listFieldWith
    , functionField
    , mapContext
    , mapContextBy

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
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail            (MonadFail)
#endif
import           Data.List                     (intercalate, tails)
import           Data.Time.Clock               (UTCTime (..))
import           Data.Time.Format              (formatTime, parseTimeM)
import           Data.Time.Locale.Compat       (TimeLocale, defaultTimeLocale)
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider
import           Hakyll.Core.Util.String       (needlePrefix, splitAll)
import           Hakyll.Web.Html
import           Prelude                       hiding (id)
import           System.FilePath               (dropExtension, splitDirectories,
                                                takeBaseName)


--------------------------------------------------------------------------------
-- | Mostly for internal usage
data ContextField
    = EmptyField
    | StringField String
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
-- | Tries to find a key in the left context,
-- or when that fails in the right context.
instance Semigroup (Context a) where
    (<>) (Context f) (Context g) = Context $ \k a i -> f k a i <|> g k a i

instance Monoid (Context a) where
    mempty  = missingField
    mappend = (<>)


--------------------------------------------------------------------------------
field' :: String -> (Item a -> Compiler ContextField) -> Context a
field' key value = Context $ \k _ i ->
    if k == key
        then value i
        else noResult $ "Tried field " ++ key


--------------------------------------------------------------------------------
-- | Constructs a new field for a 'Context'.
-- If the key matches, the compiler is run and its result is substituted in the
-- template.
--
-- If the compiler fails, the field will be considered non-existent
-- in an @$if()$@ macro or ultimately break the template application
-- (unless the key is found in another context when using '<>').
-- Use 'empty' or 'noResult' for intentional failures of fields used in
-- @$if()$@, to distinguish them from exceptions thrown with 'fail'.
field
    :: String                      -- ^ Key
    -> (Item a -> Compiler String) -- ^ Function that constructs a value based
                                   -- on the item (e.g. accessing metadata)
    -> Context a
field key value = field' key (fmap StringField . value)


--------------------------------------------------------------------------------
-- | Creates a 'field' to use with the @$if()$@ template macro.
-- Attempting to substitute the field into the template will cause an error.
boolField
    :: String           -- ^ Key
    -> (Item a -> Bool) -- ^ Extract value from an @'Item' a@
    -> Context a
boolField name f = boolFieldM name (pure . f)


--------------------------------------------------------------------------------
-- | Creates a 'field' to use with the @$if()$@ template macro, in the 
-- 'Compiler' monad. Attempting to substitute the field into the template 
-- will cause an error.
--
-- @since 4.16.4.0
boolFieldM
    :: String                       -- ^ Key
    -> (Item a -> Compiler Bool)    -- ^ Extract value from an @'Item' a@ 
                                    --   from within the 'Compiler' monad
    -> Context a
boolFieldM name f = field' name (\i -> do
  b <- f i
  if b
    then return EmptyField
    else noResult $ "Field " ++ name ++ " is false")


--------------------------------------------------------------------------------
-- | Creates a 'field' that does not depend on the 'Item' but always yields
-- the same string
constField :: String     -- ^ Key
           -> String     -- ^ Value
           -> Context a
constField key = field key . const . return


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
listFieldWith key c f = field' key $ fmap (ListField c) . f


--------------------------------------------------------------------------------
-- | Creates a variadic function field.
--
-- The function will be called with the dynamically evaluated string arguments
-- from the template as well as the page that is currently rendered.
functionField :: String                                  -- ^ Key
              -> ([String] -> Item a -> Compiler String) -- ^ Function
              -> Context a
functionField name value = Context $ \k args i ->
    if k == name
        then StringField <$> value args i
        else noResult $ "Tried function field " ++ name


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
mapContext = mapContextBy (const True)


--------------------------------------------------------------------------------
-- | Transform the respective string results of all fields in a context
-- satisfying a predicate. For example,
--
-- > mapContextBy (=="y") (++"c") (constField "x" "a" <> constField "y" "b")
--
-- is equivalent to
--
-- > constField "x" "a" <> constField "y" "bc"
--
mapContextBy :: (String -> Bool) -> (String -> String) -> Context a -> Context a
mapContextBy p f (Context c) = Context $ \k a i -> do
    fld <- c k a i
    case fld of
        EmptyField      -> wrongType "boolField"
        StringField str -> return $ StringField $
                             if p k then f str else str
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

--------------------------------------------------------------------------------
-- | A context that contains (in that order)
--
--     1. A @$body$@ 'bodyField'
--
--     2. Metadata fields
--
--     3. A @$url$@ 'urlField'
--
--     4. A @$path$@ 'pathField'
--
--     5. A @$title$@ 'titleField'
--
-- This order means that all of the fields, except @$body$@,
-- can have their values replaced by metadata fields of the same name.
-- For example, a context from a file at @posts/foo.markdown@ has a default title
-- @foo@.  However, with a metadata field:
--
-- > ---
-- > title: The Foo Story
-- > ---
--
-- The @$title$@ will be replaced with @The Foo Story@.
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
-- | Body of the item, that is, the main content of the underlying file
bodyField :: String -> Context String
bodyField key = field key $ return . itemBody


--------------------------------------------------------------------------------
-- | Map any field to its metadata value, if present
metadataField :: Context a
metadataField = Context $ \k _ i -> do
    let id = itemIdentifier i
        empty' = noResult $ "No '" ++ k ++ "' field in metadata " ++
            "of item " ++ show id
    value <- getMetadataField id k
    maybe empty' (return . StringField) value


--------------------------------------------------------------------------------
-- | Absolute url to the resulting item.  For an example item that produces a
-- file @posts/foo.html@, this field contains "posts/foo.html"
urlField :: String -> Context a
urlField key = field key $ \i -> do
    let id = itemIdentifier i
        empty' = fail $ "No route url found for item " ++ show id
    fmap (maybe empty' toUrl) $ getRoute id


--------------------------------------------------------------------------------
-- | Filepath of the underlying file of the item.  For an example
-- underlying file @posts/foo.markdown@, this field contains
-- "posts/foo.markdown"
pathField :: String -> Context a
pathField key = field key $ return . toFilePath . itemIdentifier


--------------------------------------------------------------------------------
-- | Basename of the underlying file of the item.  For an example
-- underlying file @posts/foo.markdown@, this field contains "foo"
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
--   * @06.09.2010@
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
--
-- As another alternative, if none of the above matches, and the file has a
-- path which contains nested directories specifying a date, then that date
-- will be used. In other words, if the path is of the form
-- @**//yyyy//mm//dd//**//main.extension@ .
-- As above, in case of multiple matches, the rightmost one is used.

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
getItemUTC :: (MonadMetadata m, MonadFail m)
           => TimeLocale        -- ^ Output time locale
           -> Identifier        -- ^ Input page
           -> m UTCTime         -- ^ Parsed UTCTime
getItemUTC locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = lookupString k metadata >>= parseTime' fmt
        paths          = splitDirectories $ (dropExtension . toFilePath) id'

    maybe empty' return $ msum $
        [tryField "published" fmt | fmt <- formats] ++
        [tryField "date"      fmt | fmt <- formats] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fnCand | fnCand <- reverse paths] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ fnCand | fnCand <- map (take 3) $ reverse . tails $ paths]
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC: " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTimeM True locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%a, %d %b %Y %H:%M:%S"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%dT%H:%M:%S"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S"
        , "%Y-%m-%d"
        , "%d.%m.%Y"
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
missingField = Context $ \k _ _ -> noResult $
    "Missing field '" ++ k ++ "' in context"
