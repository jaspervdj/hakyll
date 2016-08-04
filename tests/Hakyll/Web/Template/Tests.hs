--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Template.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@=?), (@?=))


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Provider
import           Hakyll.Web.Pandoc
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.Internal
import           Hakyll.Web.Template.Internal.Element
import           Hakyll.Web.Template.List
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Template.Tests" $ concat
    [ [ testCase "case01" $ test ("template.html.out", "template.html", "example.md")
      , testCase "case02" $ test ("strip.html.out", "strip.html", "example.md")
      , testCase "applyJoinTemplateList" testApplyJoinTemplateList
      ]

    , fromAssertions "readTemplate"
        [ [Chunk "Hello ", Expr (Call "guest" [])]
            @=? readTemplateElemsFile "" "Hello $guest()$"
        , [If (Call "a" [StringLiteral "bar"]) [Chunk "foo"] Nothing]
            @=? readTemplateElemsFile "" "$if(a(\"bar\"))$foo$endif$"
        -- 'If' trim check.
        , [ TrimL
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
          @=? readTemplateElemsFile "" "$-if(body)-$\n$body$\n$-else-$\n$body$\n$-endif-$"
        -- 'For' trim check.
        , [ TrimL
          , For (Ident (TemplateKey "authors"))
                [TrimR, Chunk "\n   body   \n", TrimL]
                Nothing
          , TrimR
          ]
          @=? readTemplateElemsFile "" "$-for(authors)-$\n   body   \n$-endfor-$"
        -- 'Partial' trim check.
        , [ TrimL
          , Partial (StringLiteral "path")
          , TrimR
          ]
          @=? readTemplateElemsFile "" "$-partial(\"path\")-$"
        -- 'Expr' trim check.
        , [ TrimL
          , Expr (Ident (TemplateKey "foo"))
          , TrimR
          ]
          @=? readTemplateElemsFile "" "$-foo-$"
        ]
    ]


--------------------------------------------------------------------------------
test :: (Identifier, Identifier, Identifier) -> Assertion
test (outf, tplf, itemf) = do
    store    <- newTestStore
    provider <- newTestProvider store

    out  <- resourceString provider outf
    tpl  <- testCompilerDone store provider tplf templateBodyCompiler
    item <- testCompilerDone store provider itemf $
        pandocCompiler >>= applyTemplate (itemBody tpl) testContext

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
