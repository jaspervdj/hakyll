-- | A Module that allows easy rendering of RSS feeds. If you use this module,
--   you must make sure you set the `absoluteUrl` field in the main Hakyll
--   configuration.
module Text.Hakyll.Rss
    ( RssConfiguration (..)
    , renderRss
    , renderRssWith
    ) where

import Control.Arrow ((>>>), second)
import Control.Monad.Reader (liftIO)

import Text.Hakyll.Context (ContextManipulation)
import Text.Hakyll.Hakyll (Hakyll)
import Text.Hakyll.Render (render, renderChain)
import Text.Hakyll.Renderables (createListingWith)
import Text.Hakyll.RenderAction (Renderable)

import Paths_hakyll

-- | This is a data structure to keep the configuration of an RSS feed.
data RssConfiguration = RssConfiguration
    { -- | Url of the RSS feed (relative to site root). For example, @rss.xml@.
      rssUrl         :: String
    , -- | Title of the RSS feed.
      rssTitle       :: String
    , -- | Description of the RSS feed.
      rssDescription :: String
    }

-- | This is an auxiliary function to create a listing that is, in fact, an RSS
--   feed.
createRssWith :: ContextManipulation -- ^ Manipulation to apply on the items.
              -> RssConfiguration    -- ^ Feed configuration.
              -> [Renderable]        -- ^ Items to include.
              -> FilePath            -- ^ RSS feed template.
              -> FilePath            -- ^ RSS item template.
              -> Renderable
createRssWith manipulation configuration renderables template itemTemplate =
    listing >>> render template
  where
    listing = createListingWith manipulation (rssUrl configuration)
                                [itemTemplate] renderables additional

    additional = map (second $ Left . ($ configuration))
        [ ("title", rssTitle)
        , ("description", rssDescription)
        ]

-- | Render an RSS feed with a number of items.
--
--   Note that the @Renderable@s should have the following fields:
--
--   - @$title@: Title of the item.
--
--   - @$description@: Description to appear in the feed.
--
--   - @$url@: URL to the item - this is usually set automatically.
--
renderRss :: RssConfiguration -- ^ Feed configuration.
          -> [Renderable]     -- ^ Items to include in the feed.
          -> Hakyll ()
renderRss = renderRssWith id

-- | Render an RSS feed with a number of items. This function allows you to
--   specify a @ContextManipulation@ which will be applied on every
--   @Renderable@.
renderRssWith :: ContextManipulation -- ^ Manipulation to apply on the items.
              -> RssConfiguration    -- ^ Feed configuration.
              -> [Renderable]        -- ^ Items to include in the feed.
              -> Hakyll ()
renderRssWith manipulation configuration renderables = do
    template <- liftIO $ getDataFileName "templates/rss.xml"
    itemTemplate <- liftIO $ getDataFileName "templates/rss-item.xml"
    let renderRssWith' = createRssWith manipulation configuration
                                       renderables template itemTemplate
    renderChain [] renderRssWith'
