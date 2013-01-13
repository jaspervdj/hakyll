{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Control.Monad (forM_)
import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty)
import Text.Pandoc

main :: IO ()
main = hakyllWith config $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Static directories
    forM_ ["images/*", "examples/*", "reference/**"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    -- Pages
    forM_ pages $ \p -> match p $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> requireA "sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Tutorials
    match "tutorials/*" $ do
        route   $ setExtension "html"
        compile $ pageCompilerWith defaultHakyllParserState withToc
            >>> requireA "sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
            >>> applyTemplateCompiler "templates/tutorial.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Tutorial list
    match "tutorials.html" $ route idRoute
    create "tutorials.html" $ constA mempty
        >>> arr (setField "title" "Tutorials")
        >>> setFieldPageList chronological
                "templates/tutorial-item.html" "tutorials" "tutorials/*"
        >>> requireA "sidebar.markdown" (setFieldA "sidebar" $ arr pageBody)
        >>> applyTemplateCompiler "templates/tutorials.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Sidebar
    match "sidebar.markdown" $ compile pageCompiler

    -- Templates
    match "templates/*" $ compile templateCompiler
  where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate = "$toc$\n$body$"
        , writerStandalone = True
        }

    pages = [ "about.markdown"
            , "changelog.markdown"
            , "examples.markdown"
            , "index.markdown"
            , "philosophy.markdown"
            , "reference.markdown"
            ]

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
                      \_site/* \
                      \jaspervdj@jaspervdj.be:jaspervdj.be/hakyll/hakyll3"
    }
