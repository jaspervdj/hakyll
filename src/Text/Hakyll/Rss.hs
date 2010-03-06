-- | A Module that allows easy rendering of RSS feeds.
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

data RssConfiguration = RssConfiguration
    { -- | Url of the RSS feed (relative to site root). For example, @rss.xml@.
      rssUrl         :: String
    , -- | Title of the RSS feed.
      rssTitle       :: String
    , -- | Description of the RSS feed.
      rssDescription :: String
    }

createRssWith :: ContextManipulation
              -> RssConfiguration
              -> [Renderable]
              -> FilePath
              -> FilePath
              -> Renderable
createRssWith manipulation configuration renderables template itemTemplate =
    listing >>> render template
  where
    listing = createListingWith manipulation (rssUrl configuration) itemTemplate
                                renderables additional

    additional = map (second $ Left . ($ configuration))
        [ ("title", rssTitle)
        , ("description", rssDescription)
        ]

renderRss :: RssConfiguration -> [Renderable] -> Hakyll ()
renderRss = renderRssWith id

renderRssWith :: ContextManipulation
              -> RssConfiguration
              -> [Renderable]
              -> Hakyll ()
renderRssWith manipulation configuration renderables = do
    template <- liftIO $ getDataFileName "templates/rss.xml"
    itemTemplate <- liftIO $ getDataFileName "templates/rss-item.xml"
    let renderRssWith' = createRssWith manipulation configuration
                                       renderables template itemTemplate
    renderChain [] renderRssWith'
