--------------------------------------------------------------------------------
module Hakyll.Core.Provider.Metadata.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict           as HMS
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Metadata
import           Test.Framework                (Test, testGroup)
import           Test.HUnit                    (Assertion, (@=?))
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Provider.Metadata.Tests" $
    fromAssertions "page" [testPage01, testPage02]



--------------------------------------------------------------------------------
testPage01 :: Assertion
testPage01 =
    Right (meta [("foo", "bar")], "qux\n") @=? parsePage
    "---\n\
    \foo: bar\n\
    \---\n\
    \qux\n"


--------------------------------------------------------------------------------
testPage02 :: Assertion
testPage02 =
    Right (meta [("description", descr)], "Hello I am dog\n") @=?
    parsePage
    "---\n\
    \description: A long description that would look better if it\n\
    \             spanned multiple lines and was indented\n\
    \---\n\
    \Hello I am dog\n"
  where
    descr :: String
    descr =
        "A long description that would look better if it \
        \spanned multiple lines and was indented"


--------------------------------------------------------------------------------
meta :: Yaml.ToJSON a => [(String, a)] -> Metadata
meta pairs = HMS.fromList [(T.pack k, Yaml.toJSON v) | (k, v) <- pairs]
