{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Control.Monad (forM_)
import Control.Arrow ((>>>), arr)
import Text.Pandoc (writerTableOfContents, writerTemplate, writerStandalone)

main :: IO ()
main = hakyll $ do
    route   "css/*" idRoute
    compile "css/*" defaultCompressCss

    -- Static directories
    forM_ ["images/*", "examples/*", "reference/*"] $ \f -> do
        route   f idRoute
        compile f defaultCopyFile

    -- Pages
    forM_ pages $ \p -> do
        route   p $ setExtension "html"
        compile p $ defaultPageRead
            >>> requireA "sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
            >>> defaultApplyTemplate "templates/default.html"
            >>> defaultRelativizeUrls

    -- Tutorial
    route   "tutorial.markdown" $ setExtension "html"
    compile "tutorial.markdown" $ pageRead
        >>> pageRenderPandocWith defaultParserState withToc
        >>> requireA "sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
        >>> defaultApplyTemplate "templates/default.html"
        >>> defaultRelativizeUrls

    -- Sidebar
    compile "sidebar.markdown" defaultPageRead

    -- Templates
    compile "templates/*" defaultTemplateRead
  where
    withToc = defaultWriterOptions
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
