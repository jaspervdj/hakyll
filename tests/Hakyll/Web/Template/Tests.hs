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
import           Hakyll.Core.Item
import           Hakyll.Core.Provider
import           Hakyll.Web.Pandoc
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.Internal
import           Hakyll.Web.Template.List
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Template.Tests" $ concat
    [ [ testCase "case01"                case01
      , testCase "applyJoinTemplateList" testApplyJoinTemplateList
      ]

    , fromAssertions "readTemplate"
        [ Template [Chunk "Hello ", Expr (Call "guest" [])]
            @=? readTemplate "Hello $guest()$"
        , Template
            [If (Call "a" [StringLiteral "bar"])
                (Template [Chunk "foo"])
                Nothing]
            @=? readTemplate "$if(a(\"bar\"))$foo$endif$"
        -- 'If' trim check.
        , Template
            [ TrimL
            , TrimR
            , If (Ident (TemplateKey "body"))
                 (Template [ Chunk "\n"
                           , Expr (Ident (TemplateKey "body"))
                           , Chunk "\n"
                           ])
                 (Just (Template [ TrimL
                                 , TrimR
                                 , Chunk "\n"
                                 , Expr (Ident (TemplateKey "body"))
                                 , Chunk "\n"
                                 ]))
            , TrimL
            , TrimR
            ]
            @=? readTemplate "$-if(body)-$\n$body$\n$-else-$\n$body$\n$-endif-$"
        -- 'For' trim check.
        , Template
            [ TrimL
            , TrimR
            , For (Ident (TemplateKey "authors"))
                  (Template [Chunk "\n   body   \n"])
                  Nothing
            , TrimL
            , TrimR
            ]
            @=? readTemplate "$-for(authors)-$\n   body   \n$-endfor-$"
        -- 'Partial' trim check.
        , Template
            [ TrimL
            , Partial (StringLiteral "path")
            , TrimR
            ]
            @=? readTemplate "$-partial(\"path\")-$"
        ]
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    store    <- newTestStore
    provider <- newTestProvider store

    out  <- resourceString provider "template.html.out"
    tpl  <- testCompilerDone store provider "template.html" templateBodyCompiler
    item <- testCompilerDone store provider "example.md" $
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
    tpl = Template [Chunk "<b>", Expr (Ident "body"), Chunk "</b>"]
