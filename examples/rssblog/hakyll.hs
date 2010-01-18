module Main where

import Control.Monad.Reader (liftIO)
import Text.Hakyll (hakyll, defaultHakyllConfiguration)
import Text.Hakyll.Render (renderAndConcat, renderChain, css)
import Text.Hakyll.File (getRecursiveContents, directory)
import Text.Hakyll.Renderables (createPagePath, createCustomPage)
import Data.List (sort)
import Control.Monad (mapM_, liftM)
import Data.Either (Either(..))

main = hakyll defaultHakyllConfiguration $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map createPagePath postPaths

    -- Render index, including recent posts.
    let recentPosts = renderAndConcat ["templates/postitem.html"] $ take 3 renderablePosts
    renderChain ["index.html", "templates/default.html"] $
        createCustomPage "index.html" ("templates/postitem.html" : take 3 postPaths)
            [("title", Left "Home"), ("posts", Right recentPosts)]

    -- Render all posts list.
    let postItems = renderAndConcat ["templates/postitem.html"] $ renderablePosts
    renderChain ["posts.html", "templates/default.html"] $
        createCustomPage "posts.html" ("templates/postitem.html" : postPaths)
        [("title", Left "All posts"), ("posts", Right postItems)]

    -- Render all posts.
    liftIO $ putStrLn "Generating posts..."
    mapM_ (renderChain ["templates/post.html", "templates/default.html"]) renderablePosts

    -- Render rss feed
    let recentRSSItems = renderAndConcat ["templates/rssitem.xml"] $ take 3 renderablePosts
    let rssPage = createCustomPage "rss.xml"
                        ("templates/postitem.html" : take 3 postPaths)
                        [("items", Right recentRSSItems)]
    renderChain ["templates/rss.xml"] rssPage
