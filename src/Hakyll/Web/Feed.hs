-- | A Module that allows easy rendering of RSS feeds.
--
-- The main rendering functions (@renderRss@, @renderAtom@) all assume that
-- you pass the list of items so that the most recent entry in the feed is the
-- first item in the list.
--
-- Also note that the pages should have (at least) the following fields to
-- produce a correct feed:
--
-- - @$title$@: Title of the item
--
-- - @$description$@: Description to appear in the feed
--
-- - @$url$@: URL to the item - this is usually set automatically.
--
-- In addition, the posts should be named according to the rules for
-- 'Hakyll.Page.Metadata.renderDateField'.
--
module Hakyll.Web.Feed
    ( FeedConfiguration (..)
    , renderRss
    , renderAtom
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), arr, (&&&))
import Control.Monad ((<=<))
import Data.Maybe (fromMaybe, listToMaybe)

import Hakyll.Core.Compiler
import Hakyll.Web.Page
import Hakyll.Web.Page.Metadata
import Hakyll.Web.Template
import Hakyll.Web.Template.Read.Hakyll (readTemplate)
import Hakyll.Web.Urls

import Paths_hakyll

-- | This is a data structure to keep the configuration of a feed.
data FeedConfiguration = FeedConfiguration
    { -- | Title of the feed.
      feedTitle       :: String
    , -- | Description of the feed.
      feedDescription :: String
    , -- | Name of the feed author.
      feedAuthorName  :: String
    , -- | Absolute root URL of the feed site (e.g. @http://jaspervdj.be@)
      feedRoot        :: String
    } deriving (Show, Eq)

-- | This is an auxiliary function to create a listing that is, in fact, a feed.
-- The items should be sorted on date. The @$updated@ field should be set for
-- each item.
--
createFeed :: Template           -- ^ Feed template
           -> Template           -- ^ Item template
           -> String             -- ^ URL of the feed
           -> FeedConfiguration  -- ^ Feed configuration
           -> [Page String]      -- ^ Items to include
           -> String             -- ^ Resulting feed
createFeed feedTemplate itemTemplate url configuration items =
    pageBody $ applyTemplate feedTemplate
             $ trySetField "updated"     updated
             $ trySetField "title"       (feedTitle configuration)
             $ trySetField "description" (feedDescription configuration)
             $ trySetField "authorName"  (feedAuthorName configuration)
             $ trySetField "root"        (feedRoot configuration)
             $ trySetField "url"         url
             $ fromBody body
  where
    -- Preprocess items
    items' = flip map items $ applyTemplate itemTemplate
                            . trySetField "root" (feedRoot configuration)

    -- Body: concatenated items
    body = concat $ map pageBody items'

    -- Take the first updated, which should be the most recent
    updated = fromMaybe "Unknown" $ do
        p <- listToMaybe items
        return $ getField "updated" p


-- | Abstract function to render any feed.
--
renderFeed :: FilePath                       -- ^ Feed template
           -> FilePath                       -- ^ Item template
           -> FeedConfiguration              -- ^ Feed configuration
           -> Compiler [Page String] String  -- ^ Feed compiler
renderFeed feedTemplate itemTemplate configuration =
    id &&& getRoute >>> renderFeed'
  where
    -- Arrow rendering the feed from the items and the URL
    renderFeed' = unsafeCompiler $ \(items, url) -> do
        feedTemplate' <- loadTemplate feedTemplate
        itemTemplate' <- loadTemplate itemTemplate
        let url' = toUrl $ fromMaybe noUrl url
        return $ createFeed feedTemplate' itemTemplate' url' configuration items

    -- Auxiliary: load a template from a datafile
    loadTemplate = fmap readTemplate . readFile <=< getDataFileName

    -- URL is required to have a valid field
    noUrl = error "Hakyll.Web.Feed.renderFeed: no route specified"

-- | Render an RSS feed with a number of items.
--
renderRss :: FeedConfiguration              -- ^ Feed configuration
          -> Compiler [Page String] String  -- ^ Feed compiler
renderRss configuration = arr (map (addUpdated . renderDate))
    >>> renderFeed "templates/rss.xml" "templates/rss-item.xml" configuration
  where
    renderDate = renderDateField "published" "%a, %d %b %Y %H:%M:%S UT"
                                 "No date found."

-- | Render an Atom feed with a number of items.
--
renderAtom :: FeedConfiguration              -- ^ Feed configuration
           -> Compiler [Page String] String  -- ^ Feed compiler
renderAtom configuration = arr (map (addUpdated . renderDate))
    >>> renderFeed "templates/atom.xml" "templates/atom-item.xml" configuration
  where
    renderDate = renderDateField "published" "%Y-%m-%dT%H:%M:%SZ"
                                 "No date found."

-- | Copies @$updated$@ from @$published$@ if it is not already set.
--
addUpdated :: Page a -> Page a
addUpdated page = trySetField "updated" (getField "published" page) page
