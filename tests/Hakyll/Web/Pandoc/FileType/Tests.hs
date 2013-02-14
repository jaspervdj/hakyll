--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Pandoc.FileType.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           Hakyll.Web.Pandoc.FileType
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Web.Pandoc.FileType.Tests" $
    fromAssertions "fileType"
        [ Markdown                 @=? fileType "index.md"
        , Rst                      @=? fileType "about/foo.rst"
        , LiterateHaskell Markdown @=? fileType "posts/bananas.lhs"
        , LiterateHaskell LaTeX    @=? fileType "posts/bananas.tex.lhs"
        ]
