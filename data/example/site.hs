--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "index.markdown", "code.lhs"]) $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>= requireApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
