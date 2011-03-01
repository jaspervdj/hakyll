{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Control.Monad (forM_)
import Control.Arrow ((>>>), arr)
import Text.Pandoc

main :: IO ()
main = hakyll $ do
    route   "css/*" idRoute
    compile "css/*" compressCssCompiler

    -- Static directories
    forM_ ["images/*", "examples/*", "reference/*"] $ \f -> do
        route   f idRoute
        compile f copyFileCompiler

    -- Pages
    forM_ pages $ \p -> do
        route   p $ setExtension "html"
        compile p $ pageCompiler
            >>> requireA "sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Tutorial
    route   "tutorial.markdown" $ setExtension "html"
    compile "tutorial.markdown" $ readPageCompiler
        >>> pageRenderPandocWith defaultHakyllParserState withToc
        >>> requireA "sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Sidebar
    compile "sidebar.markdown" pageCompiler

    -- Templates
    compile "templates/*" templateCompiler
  where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate = "<h2>Table of contents</h2>\n$toc$\n$body$"
        , writerStandalone = True
        }

    pages = [ "about.markdown"
            , "changelog.markdown"
            , "index.markdown"
            , "philosophy.markdown"
            , "reference.markdown"
            ]
