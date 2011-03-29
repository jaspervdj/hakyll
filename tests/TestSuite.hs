module TestSuite where

import Test.Framework (defaultMain, testGroup)

import qualified Hakyll.Core.DirectedGraph.Tests
import qualified Hakyll.Core.DependencyAnalyzer.Tests
import qualified Hakyll.Core.Identifier.Tests
import qualified Hakyll.Core.Routes.Tests
import qualified Hakyll.Web.Page.Tests
import qualified Hakyll.Web.Page.Metadata.Tests
import qualified Hakyll.Web.RelativizeUrls.Tests
import qualified Hakyll.Web.Template.Tests
import qualified Hakyll.Web.Util.Url.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Hakyll.Core.DirectedGraph.Tests"
        Hakyll.Core.DirectedGraph.Tests.tests
    , testGroup "Hakyll.Core.DependencyAnalyzer.Tests"
        Hakyll.Core.DependencyAnalyzer.Tests.tests
    , testGroup "Hakyll.Core.Identifier.Tests"
        Hakyll.Core.Identifier.Tests.tests
    , testGroup "Hakyll.Core.Routes.Tests"
        Hakyll.Core.Routes.Tests.tests
    , testGroup "Hakyll.Web.Page.Tests"
        Hakyll.Web.Page.Tests.tests
    , testGroup "Hakyll.Web.Page.Metadata.Tests"
        Hakyll.Web.Page.Metadata.Tests.tests
    , testGroup "Hakyll.Web.RelativizeUrls.Tests"
        Hakyll.Web.RelativizeUrls.Tests.tests
    , testGroup "Hakyll.Web.Template.Tests"
        Hakyll.Web.Template.Tests.tests
    , testGroup "Hakyll.Web.Util.Url.Tests"
        Hakyll.Web.Util.Url.Tests.tests
    ]
