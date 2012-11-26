--------------------------------------------------------------------------------
module Hakyll.Web.Util.Html.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework       (Test, testGroup)
import           Test.HUnit           ((@=?))


--------------------------------------------------------------------------------
import           Hakyll.Web.Util.Html
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Web.Util.Html" $ concat
    [ fromAssertions "stripTags"
        [ "foo"     @=? stripTags "<p>foo</p>"
        , "foo bar" @=? stripTags "<p>foo</p> bar"
        , "foo"     @=? stripTags "<p>foo</p"
        ]
    , fromAssertions "escapeHtml"
        [ "Me &amp; Dean" @=? escapeHtml "Me & Dean"
        , "&lt;img&gt;"   @=? escapeHtml "<img>"
        ]
    ]
