--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Hakyll.Core.Provider.Metadata.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap             as KeyMap
import qualified Data.Aeson.Key                as AK
#else
import qualified Data.HashMap.Strict           as KeyMap
import qualified Data.Text                     as T
#endif
import qualified Data.Yaml                     as Yaml
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Metadata
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit              (Assertion, assertFailure, (@=?))
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.Provider.Metadata.Tests" $
    fromAssertions "page" [testPage01, testPage02]



--------------------------------------------------------------------------------
testPage01 :: Assertion
testPage01 =
    (meta [("foo", "bar")], "qux\n") `expectRight` parsePage
    "---\nfoo: bar\n---\nqux\n"


--------------------------------------------------------------------------------
testPage02 :: Assertion
testPage02 =
    (meta [("description", descr)], "Hello I am dog\n") `expectRight`
    parsePage
    "---\ndescription: A long description that would look better if it\n             spanned multiple lines and was indented\n---\nHello I am dog\n"
  where
    descr :: String
    descr =
        "A long description that would look better if it spanned multiple lines and was indented"


--------------------------------------------------------------------------------
meta :: Yaml.ToJSON a => [(String, a)] -> Metadata
meta pairs = KeyMap.fromList [(keyFromString k, Yaml.toJSON v) | (k, v) <- pairs]
  where
#if MIN_VERSION_aeson(2,0,0)
  keyFromString = AK.fromString
#else
  keyFromString = T.pack
#endif


--------------------------------------------------------------------------------
-- | This is useful when the 'Left' side of 'Either' doesn't have an 'Eq'
-- instance.
expectRight :: (Eq b, Show a, Show b) => b -> Either a b -> Assertion
expectRight _        (Left  err) = assertFailure (show err)
expectRight expected (Right res) = expected @=? res
