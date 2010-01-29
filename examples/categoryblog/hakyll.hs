module Main where

import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.Tags (readCategoryMap)
import Text.Hakyll.File (getRecursiveContents, directory, removeSpaces)
import Text.Hakyll.Renderables (createPagePath, createCustomPage, createListingWith, createListing)
import Text.Hakyll.Context (ContextManipulation, renderDate)
import Text.Hakyll.Util (link)
import Data.List (sort)
import Data.Map (toList)
import Control.Monad (mapM_, liftM, (<=<))
import Data.Either (Either(..))

main = hakyll $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map createPagePath postPaths

    -- Read category map.
    categoryMap <- readCategoryMap "categoryMap" renderablePosts

    -- Render all posts list.
    renderPostList "posts.html" "All posts" renderablePosts

    -- Render post list per category
    mapM_ (\(category, posts) -> renderPostList (categoryToURL category) ("Posts about " ++ category) posts)
          (toList categoryMap)

    -- Render index, including recent posts.
    let index = createListingWith postManipulation "index.html"
                                  "templates/postitem.html"
                                  (take 3 renderablePosts)
                                  [ ("title", "Home")
                                  , ("categories", categoryList categoryMap)
                                  ]
    renderChain ["index.html", "templates/default.html"] index

    -- Render all posts.
    mapM_ (renderChainWith postManipulation
                           ["templates/post.html"
                           ,"templates/default.html"
                           ]) renderablePosts

    -- Render rss feed
    let rss = createListing "rss.xml" "templates/rssitem.xml" (take 3 renderablePosts) []
    renderChain ["templates/rss.xml"] rss

    where postManipulation :: ContextManipulation
          postManipulation = renderDate "date" "%B %e, %Y" "Date unknown"

          categoryToURL category = "$root/categories/" ++ removeSpaces category ++ ".html"

          categoryList = uncurry categoryListItem <=< toList

          categoryListItem category posts = "<li>" ++ link category (categoryToURL category)
                                          ++ " - " ++ show (length posts) ++ " items.</li>"

          renderPostList url title posts = do
              let list = createListingWith postManipulation url "templates/postitem.html" posts [("title", title)]
              renderChain ["posts.html", "templates/default.html"] list
