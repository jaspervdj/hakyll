-- | A Module that allows easy rendering of RSS feeds. If you use this module,
--   you must make sure you set the `absoluteUrl` field in the main Hakyll
--   configuration.
--
--   Apart from that, the main rendering functions (@renderRss@,
--   @renderRssWith@, @renderAtom@ all @renderAtomWith@) all assume that you
--   pass the list of @Renderable@s so that the most recent entry in the feed is
--   the first item in the list.
--
--   Also note that the @Renderable@s should have (at least) the following
--   fields to produce a correct feed:
--
--   - @$title@: Title of the item.
--
--   - @$description@: Description to appear in the feed.
--
--   - @$url@: URL to the item - this is usually set automatically.
--
--   Furthermore, the feed will not validate if an empty list is passed.
module Text.Hakyll.Feed
    ( FeedConfiguration (..)
    , renderRss
    , renderAtom
    ) where

import Control.Arrow ((>>>), second)
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Text.Hakyll.Context (ContextManipulation, renderDate)
import Text.Hakyll.Hakyll (Hakyll, Context)
import Text.Hakyll.Render (render, renderChain)
import Text.Hakyll.Renderables (createListing)
import Text.Hakyll.HakyllAction

import Paths_hakyll

-- | This is a data structure to keep the configuration of a feed.
data FeedConfiguration = FeedConfiguration
    { -- | Url of the feed (relative to site root). For example, @rss.xml@.
      feedUrl         :: String
    , -- | Title of the feed.
      feedTitle       :: String
    , -- | Description of the feed.
      feedDescription :: String
    , -- | Name of the feed author.
      feedAuthorName  :: String
    }

-- | This is an auxiliary function to create a listing that is, in fact, a feed.
--   The items should be sorted on date.
createFeed :: FeedConfiguration         -- ^ Feed configuration.
           -> [HakyllAction () Context] -- ^ Items to include.
           -> FilePath            -- ^ Feed template.
           -> FilePath            -- ^ Item template.
           -> HakyllAction () Context
createFeed configuration renderables template itemTemplate =
    listing >>> render template
  where
    listing = createListing (feedUrl configuration)
                            [itemTemplate] renderables additional

    additional = map (second $ Left . ($ configuration))
        [ ("title", feedTitle)
        , ("description", feedDescription)
        , ("authorName", feedAuthorName)
        ] ++ updated

    -- Take the first timestamp, which should be the most recent.
    updated = let action = createHakyllAction $
                                return . fromMaybe "foo" . M.lookup "timestamp"
                  toTuple r = ("timestamp", Right $ r >>> action)
              in map toTuple $ take 1 renderables
            

-- | Abstract function to render any feed.
renderFeed :: FeedConfiguration         -- ^ Feed configuration.
           -> [HakyllAction () Context] -- ^ Items to include in the feed.
           -> FilePath                  -- ^ Feed template.
           -> FilePath                  -- ^ Item template.
           -> Hakyll ()
renderFeed configuration renderables template itemTemplate = do
    template' <- liftIO $ getDataFileName template
    itemTemplate' <- liftIO $ getDataFileName itemTemplate
    let renderFeed' = createFeed configuration renderables
                                 template' itemTemplate'
    renderChain [] renderFeed'

-- | Render an RSS feed with a number of items.
renderRss :: FeedConfiguration         -- ^ Feed configuration.
          -> [HakyllAction () Context] -- ^ Items to include in the feed.
          -> Hakyll ()
renderRss configuration renderables =
    renderFeed configuration (map (>>> renderRssDate) renderables)
               "templates/rss.xml" "templates/rss-item.xml"
  where
    renderRssDate = renderDate "timestamp" "%a, %d %b %Y %H:%M:%S UT"
                               "No date found."

-- | Render an Atom feed with a number of items.
renderAtom :: FeedConfiguration         -- ^ Feed configuration.
           -> [HakyllAction () Context] -- ^ Items to include in the feed.
           -> Hakyll ()
renderAtom configuration renderables =
    renderFeed configuration (map (>>> renderAtomDate) renderables)
               "templates/atom.xml" "templates/atom-item.xml"
  where
    renderAtomDate = renderDate "timestamp" "%Y-%m-%dT%H:%M:%SZ"
                                            "No date found."
