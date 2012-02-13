module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Hakyll.Core.Compiler.Tests
import qualified Hakyll.Core.DependencyAnalyzer.Tests
import qualified Hakyll.Core.Identifier.Tests
import qualified Hakyll.Core.Routes.Tests
import qualified Hakyll.Core.Rules.Tests
import qualified Hakyll.Core.Store.Tests
import qualified Hakyll.Core.UnixFilter.Tests
import qualified Hakyll.Core.Util.Arrow.Tests
import qualified Hakyll.Core.Util.String.Tests
import qualified Hakyll.Web.Page.Tests
import qualified Hakyll.Web.Page.Metadata.Tests
import qualified Hakyll.Web.Template.Tests
import qualified Hakyll.Web.Urls.Tests
import qualified Hakyll.Web.Urls.Relativize.Tests
import qualified Hakyll.Web.Util.Html.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Hakyll.Core.Compiler.Tests"
        Hakyll.Core.Compiler.Tests.tests
    , testGroup "Hakyll.Core.DependencyAnalyzer.Tests"
        Hakyll.Core.DependencyAnalyzer.Tests.tests
    , testGroup "Hakyll.Core.Identifier.Tests"
        Hakyll.Core.Identifier.Tests.tests
    , testGroup "Hakyll.Core.Routes.Tests"
        Hakyll.Core.Routes.Tests.tests
    , testGroup "Hakyll.Core.Rules.Tests"
        Hakyll.Core.Rules.Tests.tests
    , testGroup "Hakyll.Core.Store.Tests"
        Hakyll.Core.Store.Tests.tests
    , testGroup "Hakyll.Core.UnixFilter.Tests"
        Hakyll.Core.UnixFilter.Tests.tests
    , testGroup "Hakyll.Core.Util.Arrow.Tests"
        Hakyll.Core.Util.Arrow.Tests.tests
    , testGroup "Hakyll.Core.Util.String.Tests"
        Hakyll.Core.Util.String.Tests.tests
    , testGroup "Hakyll.Web.Page.Tests"
        Hakyll.Web.Page.Tests.tests
    , testGroup "Hakyll.Web.Page.Metadata.Tests"
        Hakyll.Web.Page.Metadata.Tests.tests
    , testGroup "Hakyll.Web.Template.Tests"
        Hakyll.Web.Template.Tests.tests
    , testGroup "Hakyll.Web.Urls.Tests"
        Hakyll.Web.Urls.Tests.tests
    , testGroup "Hakyll.Web.Urls.Relativize.Tests"
        Hakyll.Web.Urls.Relativize.Tests.tests
    , testGroup "Hakyll.Web.Util.Html.Tests"
        Hakyll.Web.Util.Html.Tests.tests
    ]
