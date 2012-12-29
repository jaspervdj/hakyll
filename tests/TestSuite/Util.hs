--------------------------------------------------------------------------------
-- | Test utilities
module TestSuite.Util
    ( fromAssertions
    , newTestStore
    , cleanTestStore
    , withTestStore
    , newTestProvider
    , testCompiler
    , testCompilerDone
    , withTestConfiguration
    ) where


--------------------------------------------------------------------------------
import           Data.Monoid                    (mempty)
import qualified Data.Set                       as S
import           System.Directory               (removeDirectoryRecursive)
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


--------------------------------------------------------------------------------
fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [Test]       -- ^ Result tests
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]


--------------------------------------------------------------------------------
newTestStore :: IO Store
newTestStore = Store.new True "_teststore"


--------------------------------------------------------------------------------
cleanTestStore :: IO ()
cleanTestStore = removeDirectoryRecursive "_teststore"


--------------------------------------------------------------------------------
withTestStore :: (Store -> IO a) -> IO a
withTestStore f = do
    store  <- newTestStore
    result <- f store
    cleanTestStore
    return result


--------------------------------------------------------------------------------
newTestProvider :: Store -> IO Provider
newTestProvider store = newProvider store (const False) "tests/data"


--------------------------------------------------------------------------------
testCompiler :: Store -> Provider -> Identifier -> Compiler a
             -> IO (CompilerResult a)
testCompiler store provider underlying compiler = do
    logger <- Logger.new Logger.Error
    let read' = CompilerRead
            { compilerUnderlying = underlying
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
        CompilerError e     -> error $
            "TestSuite.Util.testCompilerDone: compiler " ++ show underlying ++
            " threw: " ++ e
        CompilerRequire i _ -> error $
            "TestSuite.Util.testCompilerDone: compiler " ++ show underlying ++
            " requires: " ++ show i



--------------------------------------------------------------------------------
withTestConfiguration :: (Configuration -> IO a) -> IO a
withTestConfiguration f = do
    x <- f config
    removeDirectoryRecursive $ destinationDirectory config
    removeDirectoryRecursive $ storeDirectory config
    return x
  where
    config = defaultConfiguration
        { destinationDirectory = "_testsite"
        , storeDirectory       = "_teststore"
        , providerDirectory    = "tests/data"
        , verbosity            = Logger.Error
        }
