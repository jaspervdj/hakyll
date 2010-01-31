module Main where

import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.Tags (TagMap, readCategoryMap)
import Text.Hakyll.File (getRecursiveContents, directory, removeSpaces, sortByBaseName)
import Text.Hakyll.Renderables (createPagePath, createCustomPage, createListingWith, createListing)
import Text.Hakyll.Context (ContextManipulation, renderDate)
import Text.Hakyll.Util (link)
import Data.Map (toList)
import Control.Monad (mapM_, liftM, (<=<))
import Data.Either (Either(..))

main = hakyll $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sortByBaseName) $ getRecursiveContents "posts"
    let renderablePosts = map createPagePath postPaths

    -- Read category map.
    categoryMap <- readCategoryMap "categoryMap" renderablePosts

    -- Render all posts list.
    renderPostList "posts.html" "All posts" renderablePosts

    -- Render post list per category
    mapM_ (\(category, posts) -> renderPostList (categoryToUrl category) ("Posts about " ++ category) posts)
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

          categoryToUrl category = "$root/categories/" ++ removeSpaces category ++ ".html"

          categoryList :: TagMap -> String
          categoryList = uncurry categoryListItem <=< toList

          categoryListItem category posts = "<li>" ++ link category (categoryToUrl category)
                                          ++ " - " ++ show (length posts) ++ " items.</li>"

          renderPostList url title posts = do
              let list = createListingWith postManipulation url "templates/postitem.html" posts [("title", title)]
              renderChain ["posts.html", "templates/default.html"] list
