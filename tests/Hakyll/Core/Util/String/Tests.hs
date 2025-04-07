--------------------------------------------------------------------------------
module Hakyll.Core.Util.String.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        ((@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.String
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.Util.String.Tests" $ concat
    [ fromAssertions "trim"
        [ "foo" @=? trim " foo\n\t "
        ]

    , fromAssertions "replaceAll"
        [ "foo-end"       @=? replaceAll "begin"       (const "foo")    "begin-end"
        , "begin-foo"     @=? replaceAll "end"         (const "foo")    "begin-end"
        , "no match"      @=? replaceAll "abc"         (const "foo")    "no match"
        , "foo"           @=? replaceAll ".*"          (const "foo")    "full match"
        , "empty pattern" @=? replaceAll ""            (const "foo")    "empty pattern"
        , ""              @=? replaceAll "empty input" (const "foo")    ""
        , "32 & 131"      @=? replaceAll "0x[0-9]+"    (show . readInt) "0x20 & 0x83"
        ]

    , fromAssertions "splitAll"
        [ ["a", "b", "c"]     @=? splitAll ","           "a,,b,,,c,,"
        , ["abc", "def"]      @=? splitAll "[0-9]+"      "abc123def456"
        , ["no match"]        @=? splitAll ","           "no match"
        , []                  @=? splitAll ".*"          "full match"
        , ["empty pattern"]   @=? splitAll ""            "empty pattern"
        , []                  @=? splitAll "empty input" ""
        , ["λ", "∀x.x", "hi"] @=? splitAll ", *"         "λ, ∀x.x,  hi"
        ]

    , fromAssertions "needlePrefix"
        [ Just "ab" @=? needlePrefix "cd" "abcde"
        , Just "xx" @=? needlePrefix "ab" "xxab"
        , Nothing   @=? needlePrefix "a" "xx"
        , Just "x"  @=? needlePrefix "ab" "xabxab"
        , Just ""   @=? needlePrefix "ab" "abc"
        , Just ""   @=? needlePrefix "ab" "abab"
        , Nothing   @=? needlePrefix "" ""
        ]
    ]

  where
    readInt :: String -> Int
    readInt = read
