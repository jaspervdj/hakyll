--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>= requireApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            post <- pageCompiler
            saveSnapshot "content" post
            return post
                >>= requireApplyTemplate "templates/post.html" postCtx
                >>= requireApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "archive.html" $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst) `mappend`
                    constField "title" "Archives"              `mappend`
                    defaultContext

            makeItem ""
                >>= requireApplyTemplate "templates/archive.html" archiveCtx
                >>= requireApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ -> postList (take 3 . recentFirst)

            getResourceBody
                >>= applySelf indexCtx
                >>= requireApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> [Item String]) -> Compiler String
postList preprocess = do
    posts   <- preprocess <$> requireAll "posts/*"
    itemTpl <- requireBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
