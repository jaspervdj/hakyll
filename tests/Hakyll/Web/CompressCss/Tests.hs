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
        , ""              @=? compressCss "/* abc { } ;; \n\t\r */"

          -- compress separators
        , "}"             @=? compressCss ";   }"
        , "{};"           @=? compressCss "  {  }  ;  "
          -- compress whitespace even after this curly brace
        , "}"             @=? compressCss ";   }  "
          -- but do not compress separators inside of constants
        , "\"  { } ;  \"" @=? compressCss "\"  { } ;  \""
          -- don't compress separators at the start or end of constants
        , "\" }\""        @=? compressCss "\" }\""
        , "\"{ \""        @=? compressCss "\"{ \""
          -- don't get irritated by the wrong constant terminator
        , "\"   '   \""   @=? compressCss "\"   '   \""
        , "'   \"   '"    @=? compressCss "'   \"   '"
          -- don't compress whitespace in constants in the middle of a string
        , "abc '{ '"      @=? compressCss "abc '{ '"
        , "abc \"{ \""    @=? compressCss "abc \"{ \""
          -- compress multiple semicolons
        , ";"             @=? compressCss ";;;;;;;"
        ]
    ]
