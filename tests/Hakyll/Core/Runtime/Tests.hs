--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Runtime.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           System.FilePath     ((</>))
import           Test.Framework      (Test, testGroup)
import           Test.HUnit          (Assertion, (@?=))


--------------------------------------------------------------------------------
import           Hakyll
import           Hakyll.Core.Runtime
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Runtime.Tests" $ fromAssertions "run" [case01]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = withTestConfiguration $ \config -> do
    _ <- run config $ do
        match "*.md" $ do
            route   $ setExtension "html"
            compile $ do
                body <- getResourceBody
                saveSnapshot "raw" body
                return $ renderPandoc body

        match "bodies.txt" $ route idRoute
        create "bodies.txt" $ do
            items <- requireAllSnapshots "*.md" "raw" :: Compiler [Item String]
            makeItem $ concat $ map itemBody items

    example <- readFile $ destinationDirectory config </> "example.html"
    lines example @?=  ["<p>This is an example.</p>"]

    bodies <- readFile $ destinationDirectory config </> "bodies.txt"
    head (lines bodies) @?=  "This is an example."
