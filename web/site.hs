--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow    (second)
import           Control.Monad    (filterM, forM_)
import           Data.List        (isPrefixOf, sortBy)
import           Data.Ord         (comparing)
import           Hakyll
import           System.Directory (copyFile)
import           System.FilePath  (dropTrailingPathSeparator, splitPath)
import qualified Text.Pandoc      as Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    -- Copy CHANGELOG.md here.
    preprocess $ copyFile "../CHANGELOG.md" "releases.markdown"

    -- CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Static directories
    forM_ ["images/*", "examples/*"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    -- Haddock stuff
    match "reference/**.html" $ do
        route   idRoute
        compile $ fmap (withUrls hackage) <$> getResourceString

    -- Haddock stuff
    match ("reference/**" `mappend` complement "**.html") $ do
        route   idRoute
        compile copyFileCompiler

    -- Pages
    match "*.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Tutorials
    match "tutorials/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions withToc
            >>= loadAndApplyTemplate "templates/tutorial.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Tutorial list
    create ["tutorials.html"] $ do
        route idRoute
        compile $ do
            ctx <- tutorialsCtx <$>
                sortBy (comparing itemIdentifier) <$> loadAll "tutorials/*"
            makeItem ""
                >>= loadAndApplyTemplate "templates/tutorials.html" ctx
                >>= loadAndApplyTemplate "templates/default.html"   ctx
                >>= relativizeUrls

    -- Templates
    match "templates/*" $ compile templateCompiler
  where
    withToc = defaultHakyllWriterOptions
        { Pandoc.writerTableOfContents = True
        , Pandoc.writerTemplate        = Just tocTemplate
        }

    -- When did it get so hard to compile a string to a Pandoc template?
    tocTemplate =
        either error id $ either (error . show) id $
        Pandoc.runPure $ Pandoc.runWithDefaultPartials $
        Pandoc.compileTemplate "" "$toc$\n$body$"


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
                      \_site/* \
                      \jaspervdj@jaspervdj.be:jaspervdj.be/hakyll/"
    }


--------------------------------------------------------------------------------
-- | Turns
--
-- > /usr/share/doc/ghc/html/libraries/base-4.6.0.0/Data-String.html
--
-- into
--
-- > http://hackage.haskell.org/packages/archive/base/4.6.0.0/doc/html/Data-String.html
hackage :: String -> String
hackage url
    | "/usr" `isPrefixOf` url =
        "http://hackage.haskell.org/packages/archive/" ++
        packageName ++ "/" ++ version' ++ "/doc/html/" ++ baseName
    | otherwise               = url
  where
    (packageName, version')  = second (drop 1) $ break (== '-') package
    (baseName : package : _) = map dropTrailingPathSeparator $
        reverse $ splitPath url


--------------------------------------------------------------------------------
-- | Partition tutorials into tutorial series, other articles, external articles
tutorialsCtx :: [Item String] -> Context String
tutorialsCtx tuts =
    constField "title" "Tutorials"                                    <>
    listField "main"          defaultContext (ofType "main")          <>
    listField "articles"      defaultContext (ofType "article")       <>
    listField "externals"     defaultContext (ofType "external")      <>
    listField "robertwpearce" defaultContext (ofType "robertwpearce") <>
    defaultContext
  where
    ofType ty = filterM (\item -> do
        mbType <- getMetadataField (itemIdentifier item) "type"
        return $ Just ty == mbType) tuts
