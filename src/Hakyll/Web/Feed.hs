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
module Hakyll.Web.Feed
    ( FeedConfiguration (..)
    , renderRss
    , renderRssWith
    , renderAtom
    , renderAtomWith
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                 ((<=<))
import           Data.Time.Format              (TimeLocale (..), defaultTimeLocale)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Item
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.List


--------------------------------------------------------------------------------
import           Paths_hakyll


--------------------------------------------------------------------------------
-- | This is a data structure to keep the configuration of a feed.
data FeedConfiguration = FeedConfiguration
    { -- | Title of the feed.
      feedTitle       :: String
    , -- | Description of the feed.
      feedDescription :: String
    , -- | Name of the feed author.
      feedAuthorName  :: String
    , -- | Email of the feed author.
      feedAuthorEmail :: String
    , -- | Absolute root URL of the feed site (e.g. @http://jaspervdj.be@)
      feedRoot        :: String
    } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | Abstract function to render any feed.
renderFeed :: FilePath                -- ^ Feed template
           -> FilePath                -- ^ Item template
           -> FeedConfiguration       -- ^ Feed configuration
           -> Context String          -- ^ Context for the items
           -> [Item String]           -- ^ Input items
           -> Compiler (Item String)  -- ^ Resulting item
renderFeed feedPath itemPath config itemContext items = do
    feedTpl <- loadTemplate feedPath
    itemTpl <- loadTemplate itemPath

    body <- makeItem =<< applyTemplateList itemTpl itemContext' items
    applyTemplate feedTpl feedContext body
  where
    -- Auxiliary: load a template from a datafile
    loadTemplate path = do
        file <- compilerUnsafeIO $ getDataFileName path
        unsafeReadTemplateFile file

    itemContext' = mconcat
        [ itemContext
        , constField "root" (feedRoot config)
        , constField "authorName"  (feedAuthorName config)
        , constField "authorEmail" (feedAuthorEmail config)
        ]

    feedContext = mconcat
         [ bodyField  "body"
         , constField "title"       (feedTitle config)
         , constField "description" (feedDescription config)
         , constField "authorName"  (feedAuthorName config)
         , constField "authorEmail" (feedAuthorEmail config)
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
            ListField _ _ -> fail "Hakyll.Web.Feed.renderFeed: Internal error"
            StringField s -> return s


--------------------------------------------------------------------------------
-- | Render an RSS feed with a number of items.
renderRss :: FeedConfiguration       -- ^ Feed configuration
          -> Context String          -- ^ Item context
          -> [Item String]           -- ^ Feed items
          -> Compiler (Item String)  -- ^ Resulting feed
renderRss = renderRssWith defaultTimeLocale

-- | This is an extended version of 'renderRss' that allows you to
-- specify a time locale that is used for parsing the date.
renderRssWith :: TimeLocale -> FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)
renderRssWith locale config context = renderFeed
    "templates/rss.xml" "templates/rss-item.xml" config
    (makeItemContext locale RFC822 context)


--------------------------------------------------------------------------------
-- | Render an Atom feed with a number of items.
renderAtom :: FeedConfiguration       -- ^ Feed configuration
           -> Context String          -- ^ Item context
           -> [Item String]           -- ^ Feed items
           -> Compiler (Item String)  -- ^ Resulting feed
renderAtom = renderAtomWith defaultTimeLocale

-- | This is an extended version of 'renderAtom' that allows you to
-- specify a time locale that is used for parsing the date.
renderAtomWith :: TimeLocale -> FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)
renderAtomWith locale config context = renderFeed
    "templates/atom.xml" "templates/atom-item.xml" config
    (makeItemContext locale RFC3339 context)


--------------------------------------------------------------------------------
-- | Copies @$updated$@ from @$published$@ if it is not already set.
makeItemContext :: TimeLocale -> DateFormat -> Context a -> Context a
makeItemContext locale fmt context = mconcat
    [dateFieldWith locale "published" fmt, context, dateFieldWith locale "updated" fmt]
