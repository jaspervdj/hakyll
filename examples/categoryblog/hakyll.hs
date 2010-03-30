module Main where

import Control.Arrow ((>>>), arr)
import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.Tags (readCategoryMap, withTagMap)
import Text.Hakyll.Feed (FeedConfiguration (..), renderRss)
import Text.Hakyll.File (getRecursiveContents, directory, removeSpaces, sortByBaseName)
import Text.Hakyll.CreateContext (createPage, createCustomPage, createListing)
import Text.Hakyll.ContextManipulations (renderDate, copyValue, changeValue)
import Text.Hakyll.Util (link)
import Data.Map (toList)
import Control.Monad (forM_, liftM, (<=<))
import Data.Either (Either(..))

main = hakyll "http://example.com" $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sortByBaseName) $ getRecursiveContents "posts"
    let renderablePosts = map ((>>> postManipulation) . createPage) postPaths

    -- Read category map.
    let categoryMap = readCategoryMap "categoryMap" postPaths

    -- Render all posts list.
    renderPostList "posts.html" "All posts" renderablePosts

    -- Render post list per category
    let renderListForCategory category posts =
            renderPostList (categoryToUrl category) ("Posts about " ++ category)
                           (map (>>> postManipulation) posts)
    withTagMap categoryMap renderListForCategory

    -- Render index, including recent posts.
    let categoryList' = categoryMap >>> categoryList
    let index = createListing "index.html"
                              ["templates/postitem.html"]
                              (take 3 renderablePosts)
                              [ ("title", Left "Home")
                              , ("categories", Right $ categoryList')
                              ]
    renderChain ["index.html", "templates/default.html"] index

    -- Render all posts.
    forM_ renderablePosts $ renderChain [ "templates/post.html"
                                        , "templates/default.html"
                                        ]

    -- Render rss feed
    renderRss myFeedConfiguration $
        map (>>> copyValue "body" "description") (take 3 renderablePosts)

  where
    postManipulation =   renderDate "date" "%B %e, %Y" "Date unknown"
                     >>> renderCategoryLink

    renderCategoryLink =
      changeValue "category" (\c -> link c $ categoryToUrl c)

    categoryToUrl c = "$root/categories/" ++ removeSpaces c ++ ".html"

    categoryList = arr $ uncurry categoryListItem <=< toList

    categoryListItem category posts =
        "<li>" ++ link category (categoryToUrl category)
        ++ " - " ++ show (length posts) ++ " items.</li>"

    renderPostList url title posts = do
        let list = createListing url ["templates/postitem.html"]
                                 posts [("title", Left title)]
        renderChain ["posts.html", "templates/default.html"] list

myFeedConfiguration = FeedConfiguration
    { feedUrl         = "rss.xml"
    , feedTitle       = "SimpleBlog RSS feed."
    , feedDescription = "A simple demo of an RSS feed created with Hakyll."
    , feedAuthorName  = "Jasper Van der Jeugt"
    }
