{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Control.Monad (forM_)
import Control.Arrow ((>>>), arr)
import Text.Pandoc

main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Static directories
    forM_ ["images/*", "examples/*", "reference/*"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    -- Pages
    forM_ pages $ \p -> match p $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> requireA "sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Tutorial
    match "tutorial.markdown" $ do
        route   $ setExtension "html"
        compile $ readPageCompiler
            >>> pageRenderPandocWith defaultHakyllParserState withToc
            >>> requireA "sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Sidebar
    match "sidebar.markdown" $ compile pageCompiler

    -- Templates
    match "templates/*" $ compile templateCompiler
  where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate = "<h2>Table of contents</h2>\n$toc$\n$body$"
        , writerStandalone = True
        }

    pages = [ "about.markdown"
            , "changelog.markdown"
            , "examples.markdown"
            , "index.markdown"
            , "philosophy.markdown"
            , "reference.markdown"
            ]
