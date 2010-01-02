module Main where

import Text.Hakyll (hakyll)
import Text.Hakyll.Render (renderAndConcat, renderChain, css)
import Text.Hakyll.File (getRecursiveContents, directory)
import Text.Hakyll.Renderables (createPagePath, createCustomPage)
import Data.List (sort)
import Control.Monad (mapM_, liftM)
import Data.Either (Either(..))

main = hakyll $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map createPagePath postPaths

    -- Render all posts list.
    let postItems = renderAndConcat "templates/postitem.html" $ renderablePosts
    renderChain ["posts.html", "templates/default.html"] $
        createCustomPage "posts.html" ("templates/postitem.html" : postPaths)
        [("title", Left "All posts"), ("posts", Right postItems)]

    -- Render all posts.
    putStrLn "Generating posts..."
    mapM_ (renderChain ["templates/post.html", "templates/default.html"]) renderablePosts

    -- Render rss feed
    let recentRSSItems = renderAndConcat "templates/rssitem.xml" $ take 3 renderablePosts
    let rssPage = createCustomPage "rss.xml"
                        ("templates/postitem.html" : take 3 postPaths)
                        [("items", Right recentRSSItems)]
    renderChain ["templates/rss.xml"] rssPage
