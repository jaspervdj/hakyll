--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Runtime.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString     as B
import           System.FilePath     ((</>))
import           Test.Framework      (Test, testGroup)
import           Test.HUnit          (Assertion, (@?=))


--------------------------------------------------------------------------------
import           Hakyll
import qualified Hakyll.Core.Logger  as Logger
import           Hakyll.Core.Runtime
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Runtime.Tests" $
    fromAssertions "run" [case01, case02]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    logger <- Logger.new Logger.Error
    _      <- run testConfiguration logger $ do
        match "images/*" $ do
            route idRoute
            compile copyFileCompiler

        match "*.md" $ do
            route   $ setExtension "html"
            compile $ do
                getResourceBody
                    >>= saveSnapshot "raw"
                    >>= renderPandoc

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

    cleanTestEnv


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = do
    logger <- Logger.new Logger.Error
    _      <- run testConfiguration logger $ do
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
