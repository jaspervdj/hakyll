module Main where

import Control.Arrow ((>>>))
import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.Tags (readTagMap, renderTagCloud, renderTagLinks, withTagMap)
import Text.Hakyll.Feed (FeedConfiguration (..), renderRss)
import Text.Hakyll.File (getRecursiveContents, directory, removeSpaces)
import Text.Hakyll.CreateContext (createPage, createCustomPage, createListing)
import Text.Hakyll.ContextManipulations (renderDate, copyValue)
import Data.List (sort)
import Data.Map (toList)
import Control.Monad (mapM_, liftM)
import Data.Either (Either(..))

main = hakyll "http://example.com" $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map ((>>> postManipulation) . createPage) postPaths

    -- Read tag map.
    let tagMap = readTagMap "postTags" postPaths

    -- Render all posts list.
    renderPostList "posts.html" "All posts" renderablePosts

    -- Render post list per tag
    withTagMap tagMap $ \tag posts ->
        renderPostList (tagToUrl tag) ("Posts tagged " ++ tag) (map (>>> postManipulation) posts)

    -- Render index, including recent posts.
    let tagCloud = tagMap >>> renderTagCloud tagToUrl 100 200
        index = createListing "index.html"
                              ["templates/postitem.html"]
                              (take 3 renderablePosts)
                              [ ("title", Left "Home")
                              , ("tagcloud", Right tagCloud)
                              ]
    renderChain ["index.html", "templates/default.html"] index

    -- Render all posts.
    mapM_ (renderChain ["templates/post.html"
                       ,"templates/default.html"
                       ]) renderablePosts

    -- Render rss feed
    renderRss myFeedConfiguration $
        map (>>> copyValue "body" "description") (take 3 renderablePosts)

    where postManipulation =   renderDate "date" "%B %e, %Y" "Date unknown"
                           >>> renderTagLinks tagToUrl 

          tagToUrl tag = "$root/tags/" ++ removeSpaces tag ++ ".html"

          renderPostList url title posts = do
              let list = createListing url ["templates/postitem.html"] posts [("title", Left title)]
              renderChain ["posts.html", "templates/default.html"] list

myFeedConfiguration = FeedConfiguration
    { feedUrl         = "rss.xml"
    , feedTitle       = "SimpleBlog RSS feed."
    , feedDescription = "A simple demo of an RSS feed created with Hakyll."
    , feedAuthorName  = "Jasper Van der Jeugt"
    }
