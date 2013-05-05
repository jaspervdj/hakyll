--------------------------------------------------------------------------------
module Hakyll.Core.Util.String.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework          (Test, testGroup)
import           Test.HUnit              ((@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.String
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
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
        [ "ab" @=? needlePrefix "cd" "abcde"
        , "xx" @=? needlePrefix "ab" "xxab"
        , "xx" @=? needlePrefix "a" "xx"
        , "x" @=? needlePrefix "ab" "xabxab"
        , "" @=? needlePrefix "ab" "abc"
        , "" @=? needlePrefix "ab" "abab"
        , "" @=? needlePrefix "" ""
        ]
    ]

  where
    readInt :: String -> Int
    readInt = read
