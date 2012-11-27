--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                    (defaultMain)


--------------------------------------------------------------------------------
import qualified Hakyll.Core.Dependencies.Tests
import qualified Hakyll.Core.Identifier.Tests
import qualified Hakyll.Core.Provider.Tests
import qualified Hakyll.Core.Routes.Tests
import qualified Hakyll.Core.Rules.Tests
import qualified Hakyll.Core.Runtime.Tests
import qualified Hakyll.Core.Store.Tests
import qualified Hakyll.Core.UnixFilter.Tests
import qualified Hakyll.Core.Util.String.Tests
import qualified Hakyll.Web.Template.Context.Tests
import qualified Hakyll.Web.Template.Tests
import qualified Hakyll.Web.Urls.Relativize.Tests
import qualified Hakyll.Web.Urls.Tests
import qualified Hakyll.Web.Util.Html.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ Hakyll.Core.Dependencies.Tests.tests
    , Hakyll.Core.Identifier.Tests.tests
    , Hakyll.Core.Provider.Tests.tests
    , Hakyll.Core.Routes.Tests.tests
    , Hakyll.Core.Rules.Tests.tests
    , Hakyll.Core.Runtime.Tests.tests
    , Hakyll.Core.Store.Tests.tests
    , Hakyll.Core.UnixFilter.Tests.tests
    , Hakyll.Core.Util.String.Tests.tests
    , Hakyll.Web.Template.Context.Tests.tests
    , Hakyll.Web.Template.Tests.tests
    , Hakyll.Web.Urls.Relativize.Tests.tests
    , Hakyll.Web.Urls.Tests.tests
    , Hakyll.Web.Util.Html.Tests.tests
    ]
