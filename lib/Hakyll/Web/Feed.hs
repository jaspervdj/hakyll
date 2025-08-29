{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

--------------------------------------------------------------------------------
-- | A Module that allows easy rendering of RSS feeds.
--
-- The main rendering functions (@renderRss@, @renderAtom@) all assume that
-- you pass the list of items so that the most recent entry in the feed is the
-- first item in the list.
--
-- Also note that the context should have (at least) the following fields to
-- produce a correct feed:
--
-- - @$title$@: Title of the item
--
-- - @$description$@: Description to appear in the feed
--
-- - @$url$@: URL to the item - this is usually set automatically.
--
-- In addition, the posts should be named according to the rules for
-- 'Hakyll.Web.Template.Context.dateField'
--
-- Note that for XML-based feeds (i.e. Atom and RSS) field values are not escaped!
-- However, the default 'renderRss' and 'renderAtom' functions will validate the
-- produced XML. Use the -NoValidate functions instead if you need to skip this
-- validation.
module Hakyll.Web.Feed
    ( FeedConfiguration (..)
    , renderRss
    , renderRssNoValidate
    , renderAtom
    , renderAtomNoValidate
    , renderJson
    , renderRssWithTemplates
    , renderRssWithTemplatesNoValidate
    , renderAtomWithTemplates
    , renderAtomWithTemplatesNoValidate
    , renderJsonWithTemplates
    , validateXMLFeed
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal (compilerThrow)
import           Hakyll.Core.Item
import           Hakyll.Core.Util.String       (replaceAll)
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.List


--------------------------------------------------------------------------------
import           Data.FileEmbed                (makeRelativeToProject)
import           System.FilePath               ((</>))
import           Text.Printf                   (printf)
import           Control.Exception             (displayException)
import           Text.XML                      (parseText, def)
import qualified Data.Text.Lazy as T


--------------------------------------------------------------------------------
rssTemplate :: Template
rssTemplate =
    $(makeRelativeToProject ("data" </> "templates" </> "rss.xml")
        >>= embedTemplate)

rssItemTemplate :: Template
rssItemTemplate =
    $(makeRelativeToProject ("data" </> "templates" </> "rss-item.xml")
        >>= embedTemplate)

atomTemplate :: Template
atomTemplate =
    $(makeRelativeToProject ("data" </> "templates" </> "atom.xml")
        >>= embedTemplate)

atomItemTemplate :: Template
atomItemTemplate =
    $(makeRelativeToProject ("data" </> "templates" </> "atom-item.xml")
        >>= embedTemplate)

jsonTemplate :: Template
jsonTemplate =
    $(makeRelativeToProject ("data" </> "templates" </> "feed.json")
        >>= embedTemplate)

jsonItemTemplate :: Template
jsonItemTemplate =
    $(makeRelativeToProject ("data" </> "templates" </> "feed-item.json")
        >>= embedTemplate)


--------------------------------------------------------------------------------
-- | This is a data structure to keep the configuration of a feed.
data FeedConfiguration = FeedConfiguration
    { -- | Title of the feed.
      feedTitle       :: String
    , -- | Description of the feed.
      feedDescription :: String
    , -- | Name of the feed author.
      feedAuthorName  :: String
    , -- | Email of the feed author.  Set this to the empty String to leave out
      -- the email address.
      feedAuthorEmail :: String
    , -- | Absolute root URL of the feed site (e.g. @http://jaspervdj.be@)
      feedRoot        :: String
    } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | Different types a feed can have.
data FeedType = XmlFeed | JsonFeed
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | Abstract function to render any feed.
renderFeed :: FeedType                -- ^ Feed type
           -> Template                -- ^ Default feed template
           -> Template                -- ^ Default item template
           -> FeedConfiguration       -- ^ Feed configuration
           -> Context String          -- ^ Context for the items
           -> [Item String]           -- ^ Input items
           -> Compiler (Item String)  -- ^ Resulting item
renderFeed feedType feedTpl itemTpl config itemContext items = do
    protectedItems <-
      case feedType of
        XmlFeed  -> mapM (applyFilter protectCDATA) items
        JsonFeed -> pure items
    let itemDelim = case feedType of
          XmlFeed  -> ""
          JsonFeed -> ", "

    body <- makeItem =<< applyJoinTemplateList itemDelim itemTpl itemContext' protectedItems
    applyTemplate feedTpl feedContext body
  where
    applyFilter :: (Monad m,Functor f) => (String -> String) -> f String -> m (f String)
    applyFilter tr str = return $ fmap tr str
    protectCDATA :: String -> String
    protectCDATA = replaceAll "]]>" (const "]]&gt;")

    itemContext' = mconcat
        [ escapeDescription itemContext
        , constField "root" (feedRoot config)
        , constField "authorName"  (feedAuthorName config)
        , emailField
        ]

    feedContext = mconcat
         [ bodyField  "body"
         , constField "title"       (feedTitle config)
         , constField "description" (feedDescription config)
         , constField "authorName"  (feedAuthorName config)
         , emailField
         , constField "root"        (feedRoot config)
         , urlField   "url"
         , updatedField
         , missingField
         ]

    -- Take the first "updated" field from all items -- this should be the most
    -- recent.
    updatedField = field "updated" $ \_ -> case items of
        []      -> return "Unknown"
        (x : _) -> unContext itemContext' "updated" [] x >>= \cf -> case cf of
            StringField s -> return s
            _             -> fail "Hakyll.Web.Feed.renderFeed: Internal error"

    emailField = case feedAuthorEmail config of
        ""    -> missingField
        email -> constField "authorEmail" email

    escapeDescription = case feedType of
        XmlFeed  -> id
        JsonFeed -> mapContextBy (== "description") escapeString


--------------------------------------------------------------------------------
-- | Validate that a feed contains only correct XML.
validateXMLFeed :: Item String -> Compiler (Item String)
validateXMLFeed rendered = case parseText def $ T.pack (itemBody rendered) of
      Right _ -> pure rendered
      Left err -> compilerThrow
        ["Generated feed contains invalid XML (perhaps you id not escape a metadata field?)",
          displayException err]


--------------------------------------------------------------------------------
-- | Render an RSS feed using given templates with a number of items.
--
-- The resulting feed will not be validated. Prefer to use 'renderRssWithTemplates'
-- when possible.
--
-- @since 4.16.7.0
renderRssWithTemplatesNoValidate ::
       Template                -- ^ Feed template
    -> Template                -- ^ Item template
    -> FeedConfiguration       -- ^ Feed configuration
    -> Context String          -- ^ Item context
    -> [Item String]           -- ^ Feed items
    -> Compiler (Item String)  -- ^ Resulting feed
renderRssWithTemplatesNoValidate feedTemplate itemTemplate config context = renderFeed
    XmlFeed feedTemplate itemTemplate config
    (makeItemContext "%a, %d %b %Y %H:%M:%S UT" context)


--------------------------------------------------------------------------------
-- | Render an RSS feed using given templates with a number of items.
--
-- The resulting RSS feed will be validated automatically.
renderRssWithTemplates ::
       Template                -- ^ Feed template
    -> Template                -- ^ Item template
    -> FeedConfiguration       -- ^ Feed configuration
    -> Context String          -- ^ Item context
    -> [Item String]           -- ^ Feed items
    -> Compiler (Item String)  -- ^ Resulting feed
renderRssWithTemplates feedTemplate itemTemplate config context items =
  renderRssWithTemplatesNoValidate feedTemplate itemTemplate config context items
  >>= validateXMLFeed


--------------------------------------------------------------------------------
-- | Render an Atom feed using given templates with a number of items.
--
-- The resulting feed will not be validated. Prefer to use 'renderAtomWithTemplates'
-- when possible.
--
-- @since 4.16.7.0
renderAtomWithTemplatesNoValidate ::
       Template                -- ^ Feed template
    -> Template                -- ^ Item template
    -> FeedConfiguration       -- ^ Feed configuration
    -> Context String          -- ^ Item context
    -> [Item String]           -- ^ Feed items
    -> Compiler (Item String)  -- ^ Resulting feed
renderAtomWithTemplatesNoValidate feedTemplate itemTemplate config context items = renderFeed
    XmlFeed feedTemplate itemTemplate config
    (makeItemContext "%Y-%m-%dT%H:%M:%SZ" context)
    items


--------------------------------------------------------------------------------
-- | Render an Atom feed using given templates with a number of items.
--
-- The resulting Atom feed will be validated automatically.
renderAtomWithTemplates ::
       Template                -- ^ Feed template
    -> Template                -- ^ Item template
    -> FeedConfiguration       -- ^ Feed configuration
    -> Context String          -- ^ Item context
    -> [Item String]           -- ^ Feed items
    -> Compiler (Item String)  -- ^ Resulting feed
renderAtomWithTemplates feedTemplate itemTemplate config context items =
  renderAtomWithTemplatesNoValidate feedTemplate itemTemplate config context items
  >>= validateXMLFeed


--------------------------------------------------------------------------------
-- | Render a JSON feed using given templates with a number of items.
renderJsonWithTemplates ::
       Template                -- ^ Feed template
    -> Template                -- ^ Item template
    -> FeedConfiguration       -- ^ Feed configuration
    -> Context String          -- ^ Item context
    -> [Item String]           -- ^ Feed items
    -> Compiler (Item String)  -- ^ Resulting feed
renderJsonWithTemplates feedTemplate itemTemplate config context = renderFeed
    JsonFeed feedTemplate itemTemplate config
    (makeItemContext "%Y-%m-%dT%H:%M:%SZ" context)


--------------------------------------------------------------------------------
-- | Render an RSS feed with a number of items.
--
-- The resulting feed will not be validated. Prefer to use 'renderRss'
-- when possible.
--
-- @since 4.16.7.0
renderRssNoValidate :: FeedConfiguration       -- ^ Feed configuration
          -> Context String          -- ^ Item context
          -> [Item String]           -- ^ Feed items
          -> Compiler (Item String)  -- ^ Resulting feed
renderRssNoValidate = renderRssWithTemplatesNoValidate rssTemplate rssItemTemplate


--------------------------------------------------------------------------------
-- | Render an RSS feed with a number of items.
--
-- The resulting RSS feed will be validated automatically.
renderRss :: FeedConfiguration       -- ^ Feed configuration
          -> Context String          -- ^ Item context
          -> [Item String]           -- ^ Feed items
          -> Compiler (Item String)  -- ^ Resulting feed
renderRss config context items = renderRssNoValidate config context items
  >>= validateXMLFeed


--------------------------------------------------------------------------------
-- | Render an Atom feed with a number of items.
--
-- The resulting feed will not be validated. Prefer to use 'renderAtom'
-- when possible.
--
-- @since 4.16.7.0
renderAtomNoValidate :: FeedConfiguration       -- ^ Feed configuration
           -> Context String          -- ^ Item context
           -> [Item String]           -- ^ Feed items
           -> Compiler (Item String)  -- ^ Resulting feed
renderAtomNoValidate = renderAtomWithTemplatesNoValidate atomTemplate atomItemTemplate


--------------------------------------------------------------------------------
-- | Render an Atom feed with a number of items.
--
-- The resulting Atom feed will be validated automatically.
renderAtom :: FeedConfiguration       -- ^ Feed configuration
           -> Context String          -- ^ Item context
           -> [Item String]           -- ^ Feed items
           -> Compiler (Item String)  -- ^ Resulting feed
renderAtom config context items = renderAtomNoValidate config context items
  >>= validateXMLFeed


--------------------------------------------------------------------------------
-- | Render a JSON feed with a number of items.
--
-- Items' bodies will be put into @content_html@ field of the resulting JSON;
-- the @content@ field will not be set.
renderJson :: FeedConfiguration       -- ^ Feed configuration
           -> Context String          -- ^ Item context
           -> [Item String]           -- ^ Feed items
           -> Compiler (Item String)  -- ^ Resulting feed
renderJson = renderJsonWithTemplates jsonTemplate jsonItemTemplate


--------------------------------------------------------------------------------
-- | Copies @$updated$@ from @$published$@ if it is not already set.
makeItemContext :: String -> Context a -> Context a
makeItemContext fmt context = mconcat
    [context, dateField "published" fmt, dateField "updated" fmt]


--------------------------------------------------------------------------------
-- | Escape the string according to [RFC8259 §7](https://www.rfc-editor.org/rfc/rfc8259#section-7). In other words,
--   * quotation marks and backslashes are prefixed with a backslash
--   * control characters (i.e. 0x00 - 0x1F) are escaped s.t. their
--   hex representation are prefixed with "\u00" (e.g. 0x15 -> \u0015)
--   * the rest of the characters are untouched.
escapeString :: String -> String
escapeString = flip escapeString' ""
  where
    escapeString' :: String -> ShowS
    escapeString' [] s = s
    escapeString' ('"' : cs) s = showString "\\\"" (escapeString' cs s)
    escapeString' ('\\' : cs) s = showString "\\\\" (escapeString' cs s)
    escapeString' (c : cs) s
      | c < ' ' = escapeChar c (escapeString' cs s)
      | otherwise = showChar c (escapeString' cs s)

    escapeChar :: Char -> ShowS
    escapeChar = showString . printf "\\u%04X"
