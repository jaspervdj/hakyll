--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Template.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (Assertion, testCase, (@=?),
                                               (@?=), assertBool)

import           Data.Either                    (isLeft)

--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Provider
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.Internal
import           Hakyll.Web.Template.List
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.Template.Tests" $ concat
    [ [ testCase "case01" $ test ("template.html.out", "template.html", "example.md")
      , testCase "case02" $ test ("strip.html.out", "strip.html", "example.md")
      , testCase "case03" $ test ("just-meta.html.out", "just-meta.html", "example.md")
      , testCase "applyJoinTemplateList" testApplyJoinTemplateList
      ]

    , fromAssertions "parseTemplate"
        [ Right [Chunk "Hello ", Expr (Call "guest" [])]
            @=? parse "Hello $guest()$"
        , Right [If (Call "a" [StringLiteral "bar"]) [Chunk "foo"] Nothing]
            @=? parse "$if(a(\"bar\"))$foo$endif$"
        -- 'If' trim check.
        , Right [ TrimL
          , If (Ident (TemplateKey "body"))
               [ TrimR
               , Chunk "\n"
               , Expr (Ident (TemplateKey "body"))
               , Chunk "\n"
               , TrimL
               ]
               (Just [ TrimR
                     , Chunk "\n"
                     , Expr (Ident (TemplateKey "body"))
                     , Chunk "\n"
                     , TrimL
                     ])
          , TrimR
          ]
          @=? parse "$-if(body)-$\n$body$\n$-else-$\n$body$\n$-endif-$"
        -- 'For' trim check.
        , Right [ TrimL
          , For (Ident (TemplateKey "authors"))
                [TrimR, Chunk "\n   body   \n", TrimL]
                Nothing
          , TrimR
          ]
          @=? parse "$-for(authors)-$\n   body   \n$-endfor-$"
        -- 'Partial' trim check.
        , Right [ TrimL
          , Partial (StringLiteral "path")
          , TrimR
          ]
          @=? parse "$-partial(\"path\")-$"
        -- 'Expr' trim check.
        , Right [ TrimL
          , Expr (Ident (TemplateKey "foo"))
          , TrimR
          ]
          @=? parse "$-foo-$"
        -- fail on incomplete template.
        , assertBool "did not yield error" $ isLeft $
          parse "a$b"
        -- fail on mismatched template syntax.
        , assertBool "did not fail to parse" $ isLeft $
          parse "$for(xs)$\n  <p>foo</p>\n$endif$"
        ]
    ]
  where
    parse = parseTemplateElemsFile ""


--------------------------------------------------------------------------------
test :: (Identifier, Identifier, Identifier) -> Assertion
test (outf, tplf, itemf) = do
    store    <- newTestStore
    provider <- newTestProvider store

    out  <- resourceString provider outf
    tpl  <- testCompilerDone store provider tplf templateBodyCompiler
    item <- testCompilerDone store provider itemf $
        getResourceBody
        >>= renderParagraphs
        >>= applyTemplate (itemBody tpl) testContext

    out @=? itemBody item
    cleanTestEnv


--------------------------------------------------------------------------------
testContext :: Context String
testContext = mconcat
    [ defaultContext
    , listField "authors" (bodyField "name") $ do
        n1 <- makeItem "Jan"
        n2 <- makeItem "Piet"
        return [n1, n2]
    , functionField "rev" $ \args _ -> return $ unwords $ map reverse args
    ]


--------------------------------------------------------------------------------
testApplyJoinTemplateList :: Assertion
testApplyJoinTemplateList = do
    store    <- newTestStore
    provider <- newTestProvider store
    str      <- testCompilerDone store provider "item3" $
        applyJoinTemplateList ", " tpl defaultContext [i1, i2]

    str @?= "<b>Hello</b>, <b>World</b>"
    cleanTestEnv
  where
    i1  = Item "item1" "Hello"
    i2  = Item "item2" "World"
    tpl = readTemplate "<b>$body$</b>"
