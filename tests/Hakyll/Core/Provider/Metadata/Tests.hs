--------------------------------------------------------------------------------
module Hakyll.Core.Provider.Metadata.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                (Test, testGroup)
import           Test.HUnit                    (Assertion, (@=?))
import           Text.Parsec                   as P
import           Text.Parsec.String            (Parser)


--------------------------------------------------------------------------------
import           Hakyll.Core.Provider.Metadata
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Provider.Metadata.Tests" $
    fromAssertions "page" [testPage01, testPage02, testPage03, testPage04]


--------------------------------------------------------------------------------
testPage01 :: Assertion
testPage01 = testParse page ([("foo", "bar")], "qux\n")
    "---\n\
    \foo: bar\n\
    \---\n\
    \qux\n"


--------------------------------------------------------------------------------
testPage02 :: Assertion
testPage02 = testParse page
    ([("description", descr)], "Hello I am dog\n")
    "---\n\
    \description: A long description that would look better if it\n\
    \             spanned multiple lines and was indented\n\
    \---\n\
    \Hello I am dog\n"
  where
    descr =
        "A long description that would look better if it \
        \spanned multiple lines and was indented"


--------------------------------------------------------------------------------
testPage03 :: Assertion
testPage03 = testParse page
    ([("title", "Next: Org-mode")], "Yes, this is cow\n")
    (unlines [ "#+title: Next: Org-mode"
             , "Yes, this is cow"
             ])


--------------------------------------------------------------------------------
testPage04 :: Assertion
testPage04 = testParse page
    ( [ ("tags", "org, emacs, test")
      , ("date", "2014-05-23")
      ], "\nTwenty five years ago today, the hacker Karl Koch died.\n")
    (unlines [ "#+tags: org, emacs, test"
             , "#+date: 2014-05-23"
             , ""
             , "Twenty five years ago today, the hacker Karl Koch died."
             ])


--------------------------------------------------------------------------------
testParse :: (Eq a, Show a) => Parser a -> a -> String -> Assertion
testParse parser expected input = case P.parse parser "<inline>" input of
    Left err -> error $ show err
    Right x  -> expected @=? x
