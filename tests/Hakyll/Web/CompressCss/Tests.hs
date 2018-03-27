--------------------------------------------------------------------------------
module Hakyll.Web.CompressCss.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       ((@=?))


--------------------------------------------------------------------------------
import           Hakyll.Web.CompressCss
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.CompressCss.Tests" $ concat
    [ fromAssertions "compressCss"
        [
          -- compress whitespace
          "something something" @=?
            compressCss " something  \n\t\r  something "
          -- do not compress whitespace in string tokens
        , "abc \"  \t\n\r  \" xyz" @=?
            compressCss "abc \"  \t\n\r  \" xyz"
        , "abc '  \t\n\r  ' xyz" @=?
            compressCss "abc '  \t\n\r  ' xyz"

          -- strip comments
        , "before after"  @=? compressCss "before /* abc { } ;; \n\t\r */ after"
          -- don't strip comments inside string tokens
        , "before \"/* abc { } ;; \n\t\r */\" after"
                          @=? compressCss "before \"/* abc { } ;; \n\t\r */\" after"

          -- compress separators
        , "}"             @=? compressCss ";   }"
        , ";{};"          @=? compressCss " ;  {  }  ;  "
        , "text,"         @=? compressCss "text  ,  "
        , "a>b"           @=? compressCss "a > b"
        , "a+b"           @=? compressCss "a + b"
        , "a!b"           @=? compressCss "a ! b"
          -- compress calc()
        , "calc(1px + 100%/(5 + 3) - (3px + 2px)*5)" @=? compressCss "calc( 1px + 100% / ( 5 +  3) - calc( 3px + 2px ) * 5 )"
          -- compress whitespace even after this curly brace
        , "}"             @=? compressCss ";   }  "
          -- but do not compress separators inside string tokens
        , "\"  { } ; , \"" @=? compressCss "\"  { } ; , \""
          -- don't compress separators at the start or end of string tokens
        , "\" }\""        @=? compressCss "\" }\""
        , "\"{ \""        @=? compressCss "\"{ \""
          -- don't get irritated by the wrong token delimiter
        , "\"   '   \""   @=? compressCss "\"   '   \""
        , "'   \"   '"    @=? compressCss "'   \"   '"
          -- don't compress whitespace in the middle of a string
        , "abc '{ '"      @=? compressCss "abc '{ '"
        , "abc \"{ \""    @=? compressCss "abc \"{ \""
          -- compress whitespace after colons (but not before)
        , "abc :xyz"       @=? compressCss "abc : xyz"
          -- compress multiple semicolons
        , ";"             @=? compressCss ";;;;;;;"
        ]
    ]
