module Main where

import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.Tags (readTagMap, renderTagCloud, renderTagLinks)
import Text.Hakyll.File (getRecursiveContents, directory, removeSpaces)
import Text.Hakyll.Renderables (createPagePath, createCustomPage)
import Text.Hakyll.Context (ContextManipulation, renderDate)
import Data.List (sort)
import Data.Map (toList)
import Control.Monad (mapM_, liftM)
import Data.Either (Either(..))

main = hakyll $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map createPagePath postPaths

    -- Read tag map.
    tagMap <- readTagMap postPaths

    -- Render all posts list.
    renderPostList "posts.html" "All posts" postPaths

    -- Render post list per tag
    mapM_ (\(tag, posts) -> renderPostList (tagToURL tag) ("Posts tagged " ++ tag) posts)
          (toList tagMap)

    -- Render index, including recent posts.
    let recentPosts = renderAndConcatWith postManipulation
                                          "templates/postitem.html"
                                          (take 3 renderablePosts)
    renderChain ["index.html", "templates/default.html"] $
        createCustomPage "index.html" ("templates/postitem.html" : take 3 postPaths)
            [("title", Left "Home"), ("posts", Right recentPosts)]

    -- Render all posts.
    mapM_ (renderChainWith postManipulation
                           ["templates/post.html"
                           ,"templates/default.html"
                           ]) renderablePosts

    -- Render rss feed
    let recentRSSItems = renderAndConcat "templates/rssitem.xml" $ take 3 renderablePosts
    let rssPage = createCustomPage "rss.xml"
                        ("templates/postitem.html" : take 3 postPaths)
                        [("items", Right recentRSSItems)]
    renderChain ["templates/rss.xml"] rssPage

    -- Render index.
    renderChain ["templates/default.html"] $ createPagePath "index.html"

    where postManipulation :: ContextManipulation
          postManipulation = renderDate "date" "%B %e, %Y" "Date unknown"
                           . renderTagLinks tagToURL 

          tagToURL tag = "/tags/" ++ removeSpaces tag ++ ".html"

          renderPostList url title posts = do
              let postItems = renderAndConcatWith postManipulation
                                                  "templates/postitem.html"
                                                  (map createPagePath posts)
                  customPage = createCustomPage url
                                                ("templates/postitem.html" : posts)
                                                [("title", Left title), ("posts", Right postItems)]
              renderChain ["posts.html", "templates/default.html"] customPage
