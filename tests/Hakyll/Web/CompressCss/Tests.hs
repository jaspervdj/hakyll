--------------------------------------------------------------------------------
module Hakyll.Web.CompressCss.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.Char       (toUpper)
import           Test.Framework  (Test, testGroup)
import           Test.HUnit      (assert, (@=?))


--------------------------------------------------------------------------------
import           Hakyll.Web.CompressCss
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Web.CompressCss.Tests" $ concat
    [ fromAssertions "compressCss"
        [ 
          -- compress whitespace
          " something something " @=?
            compressCss " something  \n\t\r  something "

          -- strip comments
        , "" @=?
            compressCss "/* abc { } ;; \n\t\r */"

          -- compress separators
        , "}" @=?
            compressCss ";   }"
        , "{};" @=?
            compressCss "  {  }  ;  "
        , ";" @=?
            compressCss ";;;;;;;"

          -- some real-life css
        , "a:after{content: \" (\" attr(href) \")\"}" @=?
            compressCss "a:after {  content: \" (\" attr(href) \")\"; }"
        ]
    ]
