--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_)
import           Data.Monoid   (mappend)
import           Hakyll
import           Text.Pandoc


--------------------------------------------------------------------------------
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
    match "*.markdown" $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>= requireApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Tutorials
    match "tutorials/*" $ do
        route   $ setExtension "html"
        compile $ pageCompilerWith defaultHakyllParserState withToc
            >>= requireApplyTemplate "templates/tutorial.html" defaultContext
            >>= requireApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Tutorial list
    match "tutorials.html" $ do
        route idRoute
        compile $ do
            tutorials <- requireAll "tutorials/*"
            itemTpl   <- requireBody "templates/tutorial-item.html"
            list      <- applyTemplateList itemTpl defaultContext $
                chronological tutorials

            let tutorialsCtx =
                    constField "title" "Tutorials" `mappend`
                    constField "tutorials" list    `mappend`
                    defaultContext

            makeItem ""
                >>= requireApplyTemplate "templates/tutorials.html" tutorialsCtx
                >>= requireApplyTemplate "templates/default.html" tutorialsCtx
                >>= relativizeUrls

    -- Templates
    match "templates/*" $ compile templateCompiler
  where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate = "$toc$\n$body$"
        , writerStandalone = True
        }


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { verbosity     = Debug
    , deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
                      \_site/* jaspervdj@jaspervdj.be:jaspervdj.be/hakyll"
    }
