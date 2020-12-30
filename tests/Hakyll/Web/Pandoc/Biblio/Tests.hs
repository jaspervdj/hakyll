--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
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
import           Hakyll.Web.Pandoc.Biblio
import qualified Hakyll.Core.Logger         as Logger
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.Pandoc.Biblio.Tests" $
    [ goldenTest01
    ]

--------------------------------------------------------------------------------
goldenTestsDataDir :: FilePath
goldenTestsDataDir = "tests/data/biblio"

--------------------------------------------------------------------------------
goldenTest01 :: TestTree
goldenTest01 =
    goldenVsString
        "biblio01"
        (goldenTestsDataDir </> "biblio01.golden")
        (do
            -- Code lifted from https://github.com/jaspervdj/hakyll-citeproc-example.
            logger <- Logger.new Logger.Error
            let config = testConfiguration { providerDirectory = goldenTestsDataDir }
            _ <- run config logger $ do
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
