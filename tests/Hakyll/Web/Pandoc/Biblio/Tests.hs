--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Hakyll.Web.Pandoc.Biblio.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           System.FilePath            ((</>))
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Golden          (goldenVsString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LBS


--------------------------------------------------------------------------------
import           Hakyll
import           Hakyll.Core.Runtime
import qualified Hakyll.Core.Logger         as Logger
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.Pandoc.Biblio.Tests" $
    [ goldenTest01
    , goldenTest02
    , goldenTest03
    ]

--------------------------------------------------------------------------------
goldenTestsDataDir :: FilePath
goldenTestsDataDir = "tests/data/biblio"

--------------------------------------------------------------------------------
goldenTest01 :: TestTree
goldenTest01 =
    goldenVsString
        "biblio01"
        (goldenTestsDataDir </> goldenTest)
        (do
            -- Code lifted from https://github.com/jaspervdj/hakyll-citeproc-example.
            logger <- Logger.new Logger.Error
            let config = testConfiguration { providerDirectory = goldenTestsDataDir }
            _ <- run RunModeNormal config logger $ do
                let myPandocBiblioCompiler = do
                        csl <- load "chicago.csl"
                        bib <- load "refs.bib"
                        getResourceBody >>=
                            readPandocBiblio defaultHakyllReaderOptions csl bib >>=
                            return . writePandoc

                match "default.html" $ compile templateCompiler
                match "chicago.csl" $ compile cslCompiler
                match "refs.bib"    $ compile biblioCompiler
                match "page.markdown" $ do
                    route $ setExtension "html"
                    compile $
                        myPandocBiblioCompiler >>=
                        loadAndApplyTemplate "default.html" defaultContext

            output <- fmap LBS.fromStrict $ B.readFile $
                    destinationDirectory testConfiguration </> "page.html"

            cleanTestEnv

            return output)
    
    where 
        goldenTest = 
            if pandocMajorVersion == 2
                then "cites-meijer-pandoc2.golden" 
                else "cites-meijer-pandoc3.golden"


goldenTest02 :: TestTree
goldenTest02 =
    goldenVsString
        "biblio02"
        (goldenTestsDataDir </> goldenTest)
        (do
            -- Code lifted from https://github.com/jaspervdj/hakyll-citeproc-example.
            logger <- Logger.new Logger.Error
            let config = testConfiguration { providerDirectory = goldenTestsDataDir }
            _ <- run RunModeNormal config logger $ do
                let myPandocBiblioCompiler = do
                        csl <- load "chicago.csl"
                        bib <- load "refs.yaml"
                        getResourceBody >>=
                            readPandocBiblio defaultHakyllReaderOptions csl bib >>=
                            return . writePandoc

                match "default.html" $ compile templateCompiler
                match "chicago.csl" $ compile cslCompiler
                match "refs.yaml"    $ compile biblioCompiler
                match "page.markdown" $ do
                    route $ setExtension "html"
                    compile $
                        myPandocBiblioCompiler >>=
                        loadAndApplyTemplate "default.html" defaultContext

            output <- fmap LBS.fromStrict $ B.readFile $
                    destinationDirectory testConfiguration </> "page.html"

            cleanTestEnv

            return output)
    where
        goldenTest = 
            if pandocMajorVersion == 2
                then "cites-meijer-pandoc2.golden" 
                else "cites-meijer-pandoc3.golden"

goldenTest03 :: TestTree
goldenTest03 =
    goldenVsString
        "biblio03"
        (goldenTestsDataDir </> goldenTest)
        (do
            -- Code lifted from https://github.com/jaspervdj/hakyll-citeproc-example.
            logger <- Logger.new Logger.Error
            let config = testConfiguration { providerDirectory = goldenTestsDataDir }
            _ <- run RunModeNormal config logger $ do
                let myPandocBiblioCompiler = do
                        csl <- load "chicago.csl"
                        bib1 <- load "refs.bib"
                        bib2 <- load "refs2.yaml"
                        getResourceBody >>=
                            readPandocBiblios defaultHakyllReaderOptions csl [bib1, bib2] >>=
                            return . writePandoc

                match "default.html" $ compile templateCompiler
                match "chicago.csl" $ compile cslCompiler
                match "refs.bib"    $ compile biblioCompiler
                match "refs2.yaml"    $ compile biblioCompiler
                match "cites-multiple.markdown" $ do
                    route $ setExtension "html"
                    compile $
                        myPandocBiblioCompiler >>=
                        loadAndApplyTemplate "default.html" defaultContext

            output <- fmap LBS.fromStrict $ B.readFile $
                    destinationDirectory testConfiguration </> "cites-multiple.html"

            cleanTestEnv

            return output)
    where
        goldenTest = 
            if pandocMajorVersion == 2 
                then "cites-multiple-pandoc2.golden" 
                else "cites-multiple-pandoc3.golden"

--------------------------------------------------------------------------------
pandocMajorVersion :: Int
#if MIN_VERSION_pandoc(3,0,0)
pandocMajorVersion = 3
#else
pandocMajorVersion = 2   
#endif
