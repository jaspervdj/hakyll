--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.List (sort)
import Control.Applicative ((<$>))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "code.lhs"]) $ do
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
                >>= requireApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- sort <$> getMatches "posts/*"
            post  <- requireSnapshot (head posts) "content"
            return post
                >>= requireApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
