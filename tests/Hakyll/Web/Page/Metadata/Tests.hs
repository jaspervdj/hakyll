module Hakyll.Web.Page.Metadata.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)

import qualified Data.Map as M
import Data.Monoid (mempty)
import Data.Char (toLower)

import Hakyll.Web.Page
import Hakyll.Web.Page.Metadata
import TestSuite.Util

tests :: [Test]
tests = concat $
    [ fromAssertions "getField"
        [ "bar" @=? getField "foo" (Page (M.singleton "foo" "bar") "body\n")
        , ""    @=? getField "foo" (Page M.empty "body")
        ]

    , fromAssertions "getFieldMaybe"
        [ Just "bar" @=? getFieldMaybe "foo" (Page (M.singleton "foo" "bar") "")
        , Nothing    @=? getFieldMaybe "foo" (Page M.empty "body")
        ]

    , fromAssertions "setField"
        [ (Page (M.singleton "bar" "foo") "") @=? setField "bar" "foo" mempty
        , (Page (M.singleton "bar" "foo") "") @=?
            setField "bar" "foo" (Page (M.singleton "bar" "qux") "")
        ]

    , fromAssertions "trySetField"
        [ (Page (M.singleton "bar" "foo") "") @=? trySetField "bar" "foo" mempty
        , (Page (M.singleton "bar" "qux") "") @=?
            trySetField "bar" "foo" (Page (M.singleton "bar" "qux") "")
        ]

    , fromAssertions "setFieldA"
        [ (Page (M.singleton "bar" "foo") "") @=?
            setFieldA "bar" (map toLower) (mempty, "FOO")
        ]

    , fromAssertions "copyBodyToField"
        [ (Page (M.singleton "bar" "foo") "foo") @=?
            copyBodyToField "bar" (Page M.empty "foo")
        ]

    , fromAssertions "copyBodyFromField"
        [ (Page (M.singleton "bar" "foo") "foo") @=?
            copyBodyFromField "bar" (Page (M.singleton "bar" "foo") "qux")
        ]
    ]
