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
import qualified Hakyll.Core.Logger  as Logger
import           Hakyll.Core.Runtime
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Runtime.Tests" $ fromAssertions "run" [case01]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    _ <- run testConfiguration Logger.Error $ do
        match "*.md" $ do
            route   $ setExtension "html"
            compile $ do
                getResourceBody
                    >>= saveSnapshot "raw"
                    >>= return . renderPandoc

        create ["bodies.txt"] $ do
            route idRoute
            compile $ do
                items <- loadAllSnapshots "*.md" "raw"
                makeItem $ concat $ map itemBody (items :: [Item String])

    example <- readFile $
        destinationDirectory testConfiguration </> "example.html"
    lines example @?=  ["<p>This is an example.</p>"]

    bodies <- readFile $ destinationDirectory testConfiguration </> "bodies.txt"
    head (lines bodies) @?=  "This is an example."

    cleanTestEnv
