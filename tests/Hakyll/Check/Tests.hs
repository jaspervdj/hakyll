--------------------------------------------------------------------------------
module Hakyll.Check.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit ((@=?))


--------------------------------------------------------------------------------
import           Hakyll.Check
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Check.Tests"
    -- Fragment stripping protects external URL checking from
    -- InvalidUrlException on URIs whose fragment itself contains a '#'
    -- (e.g. Matrix invite links, https://matrix.to/#/#room:server.org).
    -- See issue #1050.
    (fromAssertions "stripFragments"
        [ "https://example.com/path" @=?
            stripFragments "https://example.com/path"
        , "https://example.com/" @=?
            stripFragments "https://example.com/#anchor"
        , "https://example.com/" @=?
            stripFragments "https://example.com/?q=1#anchor"
        , "https://matrix.to/" @=?
            stripFragments "https://matrix.to/#/#roomname:matrix.my-cool-server.com"
        , "" @=?
            stripFragments ""
        ])
