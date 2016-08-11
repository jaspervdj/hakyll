--------------------------------------------------------------------------------
-- | Test utilities
module TestSuite.Util
    ( fromAssertions
    , newTestStore
    , newTestProvider
    , testCompiler
    , testCompilerDone
    , testConfiguration
    , cleanTestEnv
    ) where


--------------------------------------------------------------------------------
import           Data.List                      (intercalate)
import           Data.Monoid                    (mempty)
import qualified Data.Set                       as S
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import           Text.Printf                    (printf)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Configuration
import           Hakyll.Core.Identifier
import qualified Hakyll.Core.Logger             as Logger
import           Hakyll.Core.Provider
import           Hakyll.Core.Store              (Store)
import qualified Hakyll.Core.Store              as Store
import           Hakyll.Core.Util.File


--------------------------------------------------------------------------------
fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [Test]       -- ^ Result tests
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]


--------------------------------------------------------------------------------
newTestStore :: IO Store
newTestStore = Store.new True $ storeDirectory testConfiguration


--------------------------------------------------------------------------------
newTestProvider :: Store -> IO Provider
newTestProvider store = newProvider store (const $ return False) $
    providerDirectory testConfiguration


--------------------------------------------------------------------------------
testCompiler :: Store -> Provider -> Identifier -> Compiler a
             -> IO (CompilerResult a)
testCompiler store provider underlying compiler = do
    logger <- Logger.new Logger.Error
    let read' = CompilerRead
            { compilerConfig     = testConfiguration
            , compilerUnderlying = underlying
            , compilerProvider   = provider
            , compilerUniverse   = S.empty
            , compilerRoutes     = mempty
            , compilerStore      = store
            , compilerLogger     = logger
            }

    result <- runCompiler compiler read'
    Logger.flush logger
    return result


--------------------------------------------------------------------------------
testCompilerDone :: Store -> Provider -> Identifier -> Compiler a -> IO a
testCompilerDone store provider underlying compiler = do
    result <- testCompiler store provider underlying compiler
    case result of
        CompilerDone x _    -> return x
        CompilerError _ e   -> error $
            "TestSuite.Util.testCompilerDone: compiler " ++ show underlying ++
            " threw: " ++ intercalate "; " e
        CompilerRequire i _ -> error $
            "TestSuite.Util.testCompilerDone: compiler " ++ show underlying ++
            " requires: " ++ show i



--------------------------------------------------------------------------------
testConfiguration :: Configuration
testConfiguration = defaultConfiguration
    { destinationDirectory = "_testsite"
    , storeDirectory       = "_teststore"
    , tmpDirectory         = "_testtmp"
    , providerDirectory    = "tests/data"
    }


--------------------------------------------------------------------------------
cleanTestEnv :: IO ()
cleanTestEnv = do
    removeDirectory $ destinationDirectory testConfiguration
    removeDirectory $ storeDirectory testConfiguration
    removeDirectory $ tmpDirectory testConfiguration
