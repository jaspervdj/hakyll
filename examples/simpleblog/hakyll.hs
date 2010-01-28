module Main where

import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.Context
import Text.Hakyll.File (getRecursiveContents, directory)
import Text.Hakyll.Renderables (createPagePath, createCustomPage, createListing)
import Data.List (sort)
import Control.Monad (mapM_, liftM)
import Control.Monad.Reader (liftIO)
import Data.Either (Either(..))

main = hakyll $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map createPagePath postPaths

    -- Render index, including recent posts.
    renderChain ["index.html", "templates/default.html"] $
        createListing "index.html" "templates/postitem.html" (take 3 renderablePosts) [("title", "Home")]

    -- Render all posts list.
    renderChain ["posts.html", "templates/default.html"] $
        createListing "posts.html" "templates/postitem.html" renderablePosts [("title", "All posts")]

    -- Render all posts.
    liftIO $ putStrLn "Generating posts..."
    mapM_ (renderChain ["templates/post.html", "templates/default.html"]) renderablePosts

