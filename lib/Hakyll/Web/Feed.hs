{-# LANGUAGE TemplateHaskell #-}

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
    , renderAtom
    , renderRssWithTemplates
    , renderAtomWithTemplates
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Core.Util.String     (replaceAll)
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.List


--------------------------------------------------------------------------------
import           Data.FileEmbed              (makeRelativeToProject, embedFile)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T


--------------------------------------------------------------------------------
rssTemplate :: String
rssTemplate = T.unpack $
    T.decodeUtf8 $(makeRelativeToProject "data/templates/rss.xml" >>= embedFile)

rssItemTemplate :: String
rssItemTemplate = T.unpack $
    T.decodeUtf8 $(makeRelativeToProject "data/templates/rss-item.xml" >>= embedFile)

atomTemplate :: String
atomTemplate = T.unpack $
    T.decodeUtf8 $(makeRelativeToProject "data/templates/atom.xml" >>= embedFile)

atomItemTemplate :: String
atomItemTemplate = T.unpack $
    T.decodeUtf8 $(makeRelativeToProject "data/templates/atom-item.xml" >>= embedFile)

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
renderFeed :: String                  -- ^ Default feed template
           -> String                  -- ^ Default item template
           -> FeedConfiguration       -- ^ Feed configuration
           -> Context String          -- ^ Context for the items
           -> [Item String]           -- ^ Input items
           -> Compiler (Item String)  -- ^ Resulting item
renderFeed defFeed defItem config itemContext items = do
    feedTpl <- readTemplateFile defFeed
    itemTpl <- readTemplateFile defItem

    protectedItems <- mapM (applyFilter protectCDATA) items
    body <- makeItem =<< applyTemplateList itemTpl itemContext' protectedItems
    applyTemplate feedTpl feedContext body
  where
    applyFilter :: (Monad m,Functor f) => (String -> String) -> f String -> m (f String)
    applyFilter tr str = return $ fmap tr str
    protectCDATA :: String -> String
    protectCDATA = replaceAll "]]>" (const "]]&gt;")

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

    readTemplateFile :: String -> Compiler Template
    readTemplateFile value = pure $ template $ readTemplateElems value


--------------------------------------------------------------------------------
-- | Render an RSS feed using given templates with a number of items.
renderRssWithTemplates ::
       String                  -- ^ Feed template
    -> String                  -- ^ Item template
    -> FeedConfiguration       -- ^ Feed configuration
    -> Context String          -- ^ Item context
    -> [Item String]           -- ^ Feed items
    -> Compiler (Item String)  -- ^ Resulting feed
renderRssWithTemplates feedTemplate itemTemplate config context = renderFeed
    feedTemplate itemTemplate config
    (makeItemContext "%a, %d %b %Y %H:%M:%S UT" context)


--------------------------------------------------------------------------------
-- | Render an Atom feed using given templates with a number of items.
renderAtomWithTemplates ::
       String                  -- ^ Feed template
    -> String                  -- ^ Item template
    -> FeedConfiguration       -- ^ Feed configuration
    -> Context String          -- ^ Item context
    -> [Item String]           -- ^ Feed items
    -> Compiler (Item String)  -- ^ Resulting feed
renderAtomWithTemplates feedTemplate itemTemplate config context = renderFeed
    feedTemplate itemTemplate config
    (makeItemContext "%Y-%m-%dT%H:%M:%SZ" context)


--------------------------------------------------------------------------------
-- | Render an RSS feed with a number of items.
renderRss :: FeedConfiguration       -- ^ Feed configuration
          -> Context String          -- ^ Item context
          -> [Item String]           -- ^ Feed items
          -> Compiler (Item String)  -- ^ Resulting feed
renderRss = renderRssWithTemplates rssTemplate rssItemTemplate


--------------------------------------------------------------------------------
-- | Render an Atom feed with a number of items.
renderAtom :: FeedConfiguration       -- ^ Feed configuration
           -> Context String          -- ^ Item context
           -> [Item String]           -- ^ Feed items
           -> Compiler (Item String)  -- ^ Resulting feed
renderAtom = renderAtomWithTemplates atomTemplate atomItemTemplate


--------------------------------------------------------------------------------
-- | Copies @$updated$@ from @$published$@ if it is not already set.
makeItemContext :: String -> Context a -> Context a
makeItemContext fmt context = mconcat
    [context, dateField "published" fmt, dateField "updated" fmt]
