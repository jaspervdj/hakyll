--------------------------------------------------------------------------------
module Hakyll.Web.Html.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.Char        (toUpper)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit ((@=?))
import qualified Text.HTML.TagSoup               as TS


--------------------------------------------------------------------------------
import           Hakyll.Web.Html
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.Html.Tests" $ concat
    [ fromAssertions "demoteHeaders"
        [ "<h2>A h1 title</h2>" @=?
            demoteHeaders "<h1>A h1 title</h1>" -- Assert single-step demotion
        , "<h6>A h6 title</h6>" @=?
            demoteHeaders "<h6>A h6 title</h6>" -- Assert maximum demotion is h6
        ]

    , fromAssertions "demoteHeadersBy"
        [ "<h3>A h1 title</h3>" @=?
            demoteHeadersBy 2 "<h1>A h1 title</h1>"
        , "<h6>A h5 title</h6>" @=?
            demoteHeadersBy 2 "<h5>A h5 title</h5>" -- Assert that h6 is the lowest possible demoted header.
        , "<h4>A h4 title</h4>" @=?
            demoteHeadersBy 0 "<h4>A h4 title</h4>" -- Assert that a demotion of @N < 1@ is a no-op.
        ]

    , fromAssertions "getUrls"
        [ ["/image1.png", "/image2.jpeg", "https://example.com", "/game.swf", "/poster.jpeg"] @=?
            getUrls [
                TS.TagOpen "img" [("src", "/image1.png")]
              , TS.TagOpen "img" [("src", "/image2.jpeg")]
              , TS.TagOpen "a" [("href", "https://example.com")]
              , TS.TagOpen "object" [("data", "/game.swf")]
              , TS.TagOpen "video" [("poster", "/poster.jpeg")]
              ]
        , ["/image1.png", "/image2.jpeg", "/image3.bmp"] @=?
            getUrls [ TS.TagOpen "img" [("srcset", "/image1.png 10w, /image2.jpeg, /image3.bmp 1.3x")] ]

        -- Invalid srcset specification means no URLs are extracted
        , [] @=?
            getUrls [ TS.TagOpen "img" [("srcset", "/image1.png 10wide, /image2.jpeg, /image3.bmp 1.3px")] ]
        ]

    , fromAssertions "withUrls"
        [ "<a href=\"FOO\">bar</a>" @=?
            withUrls (map toUpper) "<a href=\"foo\">bar</a>"
        , "<img src=\"OH BAR\" />" @=?
            withUrls (map toUpper) "<img src=\"oh bar\" />"

        -- Test escaping
        , "<script>\"sup\"</script>" @=?
            withUrls id "<script>\"sup\"</script>"
        , "<code>&lt;stdio&gt;</code>" @=?
            withUrls id "<code>&lt;stdio&gt;</code>"
        , "<style>body > p { line-height: 1.3 }</style>" @=?
            withUrls id "<style>body > p { line-height: 1.3 }</style>"

        -- Test minimizing elements
        , "<meta bar=\"foo\" />" @=?
            withUrls id "<meta bar=\"foo\" />"

        -- Test that URLs are extracted from img's srcset
        , "<img srcset=\"foo 200w\" />" @=?
            withUrls (const "foo") "<img srcset=\"/path/to/image.png 200w\" />"
        , "<img srcset=\"bar 200w, bar 400w\" />" @=?
            withUrls (const "bar") "<img srcset=\"/small.jpeg 200w, /img/large.jpeg 400w\" />"

        -- Invalid srcsets are left unchanged
        , "<img srcset=\"/image1.png 200px\" />" @=?
            withUrls (const "bar") "<img srcset=\"/image1.png 200px\" />"
        ]

    , fromAssertions "toUrl"
        [ "/foo/bar.html"                     @=? toUrl "foo/bar.html"
        , "/foo/bar.html"                     @=? toUrl "foo\\bar.html" -- Windows-specific
        , "/"                                 @=? toUrl "/"
        , "/funny-pics.html"                  @=? toUrl "/funny-pics.html"
        , "/funny%20pics.html"                @=? toUrl "funny pics.html"
        -- Test various reserved characters (RFC 3986, section 2.2)
        , "/%21%2A%27%28%29%3B%3A%40%26.html" @=? toUrl "/!*'();:@&.html"
        , "/%3D%2B%24%2C/%3F%23%5B%5D.html"   @=? toUrl "=+$,/?#[].html"
        -- Test various characters that are nor reserved, nor unreserved.
        , "/%E3%81%82%F0%9D%90%87%E2%88%80"   @=? toUrl "\12354\119815\8704"
        ]

    , fromAssertions "toSiteRoot"
        [ ".."    @=? toSiteRoot "/foo/bar.html"
        , "."     @=? toSiteRoot "index.html"
        , "."     @=? toSiteRoot "/index.html"
        , "../.." @=? toSiteRoot "foo/bar/qux"
        , ".."    @=? toSiteRoot "./foo/bar.html"
        , ".."    @=? toSiteRoot "/foo/./bar.html"
        ]

    , fromAssertions "isExternal"
        [ True  @=? isExternal "http://reddit.com"
        , True  @=? isExternal "https://mail.google.com"
        , True  @=? isExternal "//ajax.googleapis.com"
        , False @=? isExternal "../header.png"
        , False @=? isExternal "/foo/index.html"
        ]

    , fromAssertions "stripTags"
        [ "foo"     @=? stripTags "<p>foo</p>"
        , "foo bar" @=? stripTags "<p>foo</p> bar"
        , "foo"     @=? stripTags "<p>foo</p"
        ]

    , fromAssertions "escapeHtml"
        [ "Me &amp; Dean" @=? escapeHtml "Me & Dean"
        , "&lt;img&gt;"   @=? escapeHtml "<img>"
        ]
    ]
