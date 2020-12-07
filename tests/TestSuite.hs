--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------
import qualified Hakyll.Core.Dependencies.Tests
import qualified Hakyll.Core.Identifier.Tests
import qualified Hakyll.Core.Provider.Metadata.Tests
import qualified Hakyll.Core.Provider.Tests
import qualified Hakyll.Core.Routes.Tests
import qualified Hakyll.Core.Rules.Tests
import qualified Hakyll.Core.Runtime.Tests
import qualified Hakyll.Core.Store.Tests
import qualified Hakyll.Core.UnixFilter.Tests
import qualified Hakyll.Core.Util.String.Tests
import qualified Hakyll.Web.CompressCss.Tests
import qualified Hakyll.Web.Html.RelativizeUrls.Tests
import qualified Hakyll.Web.Html.Tests
#ifdef USE_PANDOC
import qualified Hakyll.Web.Pandoc.Biblio.Tests
import qualified Hakyll.Web.Pandoc.FileType.Tests
#endif
import qualified Hakyll.Web.Template.Context.Tests
import qualified Hakyll.Web.Template.Tests
import qualified Hakyll.Web.Tags.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hakyll"
    [ Hakyll.Core.Dependencies.Tests.tests
    , Hakyll.Core.Identifier.Tests.tests
    , Hakyll.Core.Provider.Metadata.Tests.tests
    , Hakyll.Core.Provider.Tests.tests
    , Hakyll.Core.Routes.Tests.tests
    , Hakyll.Core.Rules.Tests.tests
    , Hakyll.Core.Runtime.Tests.tests
    , Hakyll.Core.Store.Tests.tests
    , Hakyll.Core.UnixFilter.Tests.tests
    , Hakyll.Core.Util.String.Tests.tests
    , Hakyll.Web.CompressCss.Tests.tests
    , Hakyll.Web.Html.RelativizeUrls.Tests.tests
    , Hakyll.Web.Html.Tests.tests
#ifdef USE_PANDOC
    , Hakyll.Web.Pandoc.Biblio.Tests.tests
    , Hakyll.Web.Pandoc.FileType.Tests.tests
#endif
    , Hakyll.Web.Tags.Tests.tests
    , Hakyll.Web.Template.Context.Tests.tests
    , Hakyll.Web.Template.Tests.tests
    ]
