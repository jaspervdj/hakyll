{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Urls.Relativize.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)

import Hakyll.Web.Urls.Relativize
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "relativizeUrls"
    [ "<a href=\"../foo\">bar</a>" @=?
        relativizeUrls ".." "<a href=\"/foo\">bar</a>"
    , "<img src=\"../../images/lolcat.png\"></img>" @=?
        relativizeUrls "../.." "<img src=\"/images/lolcat.png\" />"
    , "<a href=\"http://haskell.org\">Haskell</a>" @=?
        relativizeUrls "../.." "<a href=\"http://haskell.org\">Haskell</a>"
    , "<a href=\"http://haskell.org\">Haskell</a>" @=?
        relativizeUrls "../.." "<a href=\"http://haskell.org\">Haskell</a>"
    , "<script src=\"//ajax.googleapis.com/jquery.min.js\"></script>" @=?
        relativizeUrls "../.."
            "<script src=\"//ajax.googleapis.com/jquery.min.js\"></script>"
    ]
