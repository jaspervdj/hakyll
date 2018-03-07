--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow    (second)
import           Control.Monad    (forM_)
import           Data.Char        (isDigit)
import           Data.List        (isPrefixOf, sortBy)
import           Data.Monoid      ((<>))
import           Data.Ord         (comparing)
import           Hakyll
import           System.Directory (copyFile)
import           System.FilePath  (dropTrailingPathSeparator, splitPath)
import           Text.Pandoc


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
            tuts <-
                sortBy (comparing itemIdentifier) <$> loadAll "tutorials/*"

            let tutorialsCtx =
                    constField "title" "Tutorials"  `mappend`
                    listField "tutorials"   tutorialCtx (return tuts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tutorials.html" tutorialsCtx
                >>= loadAndApplyTemplate "templates/default.html" tutorialsCtx
                >>= relativizeUrls

    -- Templates
    match "templates/*" $ compile templateCompiler
  where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate        = Just "$toc$\n$body$"
        }


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
data TutorialType = SeriesTutorial | ArticlesTutorial | ExternalTutorial
    deriving (Eq)


--------------------------------------------------------------------------------
-- | Partition tutorials into tutorial series, other articles, external articles
tutorialCtx :: Context String
tutorialCtx =
    field "isSeries"   (isTutorialType SeriesTutorial)   <>
    field "isArticle"  (isTutorialType ArticlesTutorial) <>
    field "isExternal" (isTutorialType ExternalTutorial) <>
    defaultContext
  where
    getTutorialType item = do
        mbExternal <- getMetadataField (itemIdentifier item) "external"
        return $ case mbExternal of
            Just _ -> ExternalTutorial
            _      -> case splitPath (toFilePath $ itemIdentifier item) of
                [_, (x : _)] -> if isDigit x then SeriesTutorial else ArticlesTutorial
                _            -> ArticlesTutorial

    isTutorialType ty0 item = do
        ty1 <- getTutorialType item
        if ty0 == ty1 then return "yes" else fail "no"
