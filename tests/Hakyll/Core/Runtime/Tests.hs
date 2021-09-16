--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Runtime.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Monad       (void)
import qualified Data.ByteString     as B
import           System.FilePath     ((</>))
import           System.Exit         (ExitCode (..))
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.HUnit    (Assertion, (@?=))


--------------------------------------------------------------------------------
import           Hakyll
import qualified Hakyll.Core.Logger  as Logger
import           Hakyll.Core.Runtime
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.Runtime.Tests" $
    fromAssertions "run" [case01, case02, case03, case04, case05, case06]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    logger <- Logger.new Logger.Error
    _      <- run RunModeNormal testConfiguration logger $ do
        match "images/*" $ do
            route idRoute
            compile copyFileCompiler

        match "*.md" $ do
            route   $ setExtension "html"
            compile $ do
                getResourceBody
                    >>= saveSnapshot "raw"
                    >>= renderParagraphs

        match (fromList ["partial.html", "partial-helper.html"]) $
            compile templateCompiler
        create ["partial.html.out"] $ do
            route idRoute
            compile $ do
                example <- loadSnapshotBody "example.md" "raw"
                makeItem example
                    >>= loadAndApplyTemplate "partial.html" defaultContext

        create ["bodies.txt"] $ do
            route idRoute
            compile $ do
                items <- loadAllSnapshots "*.md" "raw"
                makeItem $ concat $ map itemBody (items :: [Item String])

    favicon <- B.readFile $
        providerDirectory testConfiguration </> "images/favicon.ico"
    favicon' <- B.readFile $
        destinationDirectory testConfiguration </> "images/favicon.ico"
    favicon @?= favicon'

    example <- readFile $
        destinationDirectory testConfiguration </> "example.html"
    lines example @?=  ["<p>This is an example.</p>"]

    bodies <- readFile $ destinationDirectory testConfiguration </> "bodies.txt"
    head (lines bodies) @?=  "This is an example."

    partial  <- readFile $ providerDirectory    testConfiguration </> "partial.html.out"
    partial' <- readFile $ destinationDirectory testConfiguration </> "partial.html.out"
    partial @?= partial'

    cleanTestEnv


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = do
    logger <- Logger.new Logger.Error
    _      <- run RunModeNormal testConfiguration logger $ do
        match "images/favicon.ico" $ do
            route   $ gsubRoute "images/" (const "")
            compile $ makeItem ("Test" :: String)

        match "images/**" $ do
            route   idRoute
            compile copyFileCompiler

    favicon <- readFile $
        destinationDirectory testConfiguration </> "favicon.ico"
    favicon @?= "Test"

    cleanTestEnv


--------------------------------------------------------------------------------
-- Test that dependency cycles are correctly identified
case03 :: Assertion
case03 = do
    logger  <- Logger.new Logger.Error
    (ec, _) <- run RunModeNormal testConfiguration logger $ do

        create ["partial.html.out1"] $ do
            route idRoute
            compile $ do
                example <- loadBody "partial.html.out2"
                makeItem example
                    >>= loadAndApplyTemplate "partial.html" defaultContext

        create ["partial.html.out2"] $ do
            route idRoute
            compile $ do
                example <- loadBody "partial.html.out1"
                makeItem example
                    >>= loadAndApplyTemplate "partial.html" defaultContext


    ec @?= ExitFailure 1

    cleanTestEnv


--------------------------------------------------------------------------------
-- Test that dependency cycles are correctly identified when snapshots 
-- are also involved. See issue #878.
case04 :: Assertion
case04 = do
    logger  <- Logger.new Logger.Error
    (ec, _) <- run RunModeNormal testConfiguration logger $ do

        create ["partial.html.out1"] $ do
            route idRoute
            compile $ do
                example <- loadSnapshotBody "partial.html.out2" "raw"
                makeItem example
                    >>= loadAndApplyTemplate "partial.html" defaultContext

        create ["partial.html.out2"] $ do
            route idRoute
            compile $ do
                example <- loadSnapshotBody "partial.html.out1" "raw"
                makeItem example
                    >>= loadAndApplyTemplate "partial.html" defaultContext

    ec @?= ExitFailure 1

    cleanTestEnv


--------------------------------------------------------------------------------
-- Test that dependency cycles are correctly identified in the presence of 
-- snapshots. See issue #878.
case05 :: Assertion 
case05 = do
    logger  <- Logger.new Logger.Error 
    (ec, _) <- run RunModeNormal testConfiguration logger $ do

        match "posts/*" $ do
            route $ setExtension "html"
            compile $ do
                let applyDefaultTemplate item = do
                        footer <- loadBody "footer.html"
                        let postCtx' =
                                constField "footer" footer `mappend`
                                defaultContext
                        loadAndApplyTemplate "template-empty.html" postCtx' item

                pandocCompiler
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate "template-empty.html" defaultContext
                    >>= applyDefaultTemplate
                    >>= relativizeUrls

        create ["footer.html"] $
            compile $ do
                posts <- fmap (take 5) . recentFirst =<< loadAllSnapshots "posts/*" "content"
                let footerCtx =
                        listField "posts" defaultContext (return posts) `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "template-empty.html" footerCtx
        
        create ["template-empty.html"] $ compile templateCompiler

    ec @?= ExitSuccess 

    cleanTestEnv


--------------------------------------------------------------------------------
-- Test that dependency cycles are correctly identified in the presence of 
-- snapshots. The test case below was presented as an example which invalidated
-- a previous approach to dependency cycle checking. 
-- See https://github.com/jaspervdj/hakyll/pull/880#discussion_r708650172
case06 :: Assertion
case06 = do
    logger  <- Logger.new Logger.Error
    (ec, _) <- run RunModeNormal testConfiguration logger $ do

        create ["one.html"] $ do
            route idRoute
            compile $ do
                void $ makeItem ("one-one" :: String) >>= saveSnapshot "one"
                _ <- loadSnapshotBody "two.html" "two" :: Compiler String
                void $ makeItem ("one-three" :: String) >>= saveSnapshot "three"
                makeItem ("one-two" :: String) >>= saveSnapshot "two"

        create ["two.html"] $ do
            route idRoute
            compile $ do
                _ <- loadSnapshotBody "one.html" "one" :: Compiler String
                void $ makeItem ("two-two" :: String) >>= saveSnapshot "two"
                text <- loadSnapshotBody "one.html" "two"
                makeItem (text :: String)

    ec @?= ExitSuccess

    cleanTestEnv