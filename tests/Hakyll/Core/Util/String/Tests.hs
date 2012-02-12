module Hakyll.Core.Util.String.Tests
    ( tests
    ) where

import Test.Framework (Test)
import Test.HUnit ((@=?))

import Hakyll.Core.Util.String
import TestSuite.Util

tests :: [Test]
tests = concat
    [ fromAssertions "trim"
        [ "foo" @=? trim " foo\n\t "
        ]

    , fromAssertions "replaceAll"
        [ "32 & 131" @=? replaceAll "0x[0-9]+" (show . readInt) "0x20 & 0x83"
        ]

    , fromAssertions "splitAll"
        [ ["λ", "∀x.x", "hi"] @=? splitAll ", *" "λ, ∀x.x,  hi"
        ]
    ]

  where
    readInt :: String -> Int
    readInt = read
