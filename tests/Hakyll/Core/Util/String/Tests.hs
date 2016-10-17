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
        [ "32 & 131" @=? replaceAll "0x[0-9]+" (show . readInt) "0x20 & 0x83"
        ]

    , fromAssertions "splitAll"
        [ ["λ", "∀x.x", "hi"] @=? splitAll ", *" "λ, ∀x.x,  hi"
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
