--------------------------------------------------------------------------------
-- | Test utilities
module TestSuite.Util
    ( fromAssertions
    , withTestStore
    , newTestProvider
    , testCompiler
    , testCompilerDone
    ) where


--------------------------------------------------------------------------------
import           Data.Monoid                    (mempty)
import           System.Directory               (removeDirectoryRecursive)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import qualified Hakyll.Core.Logger             as Logger
import           Hakyll.Core.Provider
import           Hakyll.Core.Store              (Store)
import qualified Hakyll.Core.Store              as Store


--------------------------------------------------------------------------------
fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [Test]       -- ^ Result tests
fromAssertions name = zipWith testCase names
  where
    names = map (\n -> name ++ " [" ++ show n ++ "]") [1 :: Int ..]


--------------------------------------------------------------------------------
withTestStore :: (Store -> IO a) -> IO a
withTestStore f = do
    store  <- Store.new True "_teststore"
    result <- f store
    removeDirectoryRecursive "_teststore"
    return result


--------------------------------------------------------------------------------
newTestProvider :: Store -> IO Provider
newTestProvider store = newProvider store (const False) "tests/data"


--------------------------------------------------------------------------------
testCompiler :: Store -> Provider -> Identifier -> Compiler a
             -> IO (CompilerResult a)
testCompiler store provider underlying compiler = do
    logger <- Logger.new Logger.Debug (\_ -> return ())
    let read' = CompilerRead
            { compilerUnderlying = underlying
            , compilerProvider   = provider
            , compilerUniverse   = []
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
