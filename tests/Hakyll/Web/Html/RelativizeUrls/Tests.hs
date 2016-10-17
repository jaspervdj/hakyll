--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Html.RelativizeUrls.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               ((@=?))


--------------------------------------------------------------------------------
import           Hakyll.Web.Html.RelativizeUrls
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.Html.RelativizeUrls.Tests" $
    fromAssertions "relativizeUrls"
        [ "<a href=\"../foo\">bar</a>" @=?
            relativizeUrlsWith ".." "<a href=\"/foo\">bar</a>"
        , "<img src=\"../../images/lolcat.png\" />" @=?
            relativizeUrlsWith "../.." "<img src=\"/images/lolcat.png\" />"
        , "<video poster=\"../../images/lolcat.png\"></video>" @=?
            relativizeUrlsWith "../.."
                "<video poster=\"/images/lolcat.png\"></video>"
        , "<a href=\"http://haskell.org\">Haskell</a>" @=?
            relativizeUrlsWith "../.."
                "<a href=\"http://haskell.org\">Haskell</a>"
        , "<a href=\"http://haskell.org\">Haskell</a>" @=?
            relativizeUrlsWith "../.."
                "<a href=\"http://haskell.org\">Haskell</a>"
        , "<script src=\"//ajax.googleapis.com/jquery.min.js\"></script>" @=?
            relativizeUrlsWith "../.."
                "<script src=\"//ajax.googleapis.com/jquery.min.js\"></script>"
        ]
