module Main where

import Control.Monad.Reader (liftIO)
import Text.Hakyll (hakyll)
import Text.Hakyll.Render (renderAndConcat, renderChain, css)
import Text.Hakyll.File (getRecursiveContents, directory)
import Text.Hakyll.Renderables (createPagePath, createCustomPage, createListing)
import Data.List (sort)
import Control.Monad (mapM_, liftM)
import Data.Either (Either(..))

main = hakyll $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map createPagePath postPaths

    -- Render index, including recent posts.
    let index = createListing "index.html" "templates/postitem.html" (take 3 renderablePosts) [("title", "Home")]
    renderChain ["index.html", "templates/default.html"] index

    -- Render all posts list.
    let posts = createListing "posts.html" "templates/postitem.html" renderablePosts [("title", "All posts")]
    renderChain ["posts.html", "templates/default.html"] posts

    -- Render all posts.
    liftIO $ putStrLn "Generating posts..."
    mapM_ (renderChain ["templates/post.html", "templates/default.html"]) renderablePosts

    -- Render rss feed
    let rss = createListing "rss.xml" "templates/rssitem.xml" (take 3 renderablePosts) []
    renderChain ["templates/rss.xml"] rss
