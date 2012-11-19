--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import Test.Framework (defaultMain)


--------------------------------------------------------------------------------
import qualified Hakyll.Core.Dependencies.Tests
import qualified Hakyll.Core.Provider.Tests
import qualified Hakyll.Web.Template.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ Hakyll.Core.Dependencies.Tests.tests
    , Hakyll.Core.Provider.Tests.tests
    , Hakyll.Web.Template.Tests.tests
    ]
