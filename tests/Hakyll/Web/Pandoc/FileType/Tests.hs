--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Pandoc.FileType.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           ((@=?))


--------------------------------------------------------------------------------
import           Hakyll.Web.Pandoc.FileType
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.Pandoc.FileType.Tests" $
    fromAssertions "fileType"
        [ Markdown                 @=? fileType "index.md"
        , Rst                      @=? fileType "about/foo.rst"
        , LiterateHaskell Markdown @=? fileType "posts/bananas.lhs"
        , LiterateHaskell LaTeX    @=? fileType "posts/bananas.tex.lhs"
        ]
