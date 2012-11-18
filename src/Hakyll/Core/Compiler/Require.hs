--------------------------------------------------------------------------------
module Hakyll.Core.Compiler.Require
    ( save
    , require
    , requireBody
    , requireAll
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import           Data.Binary                    (Binary)
import           Data.Typeable


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Store              (Store)
import qualified Hakyll.Core.Store              as Store


--------------------------------------------------------------------------------
save :: (Binary a, Typeable a) => Store -> Identifier -> a -> IO ()
save store identifier x = Store.set store (key identifier) x


--------------------------------------------------------------------------------
require :: (Binary a, Typeable a) => Identifier -> Compiler (Item a)
require id' = do
    store <- compilerStore <$> compilerAsk

    compilerTellDependencies [IdentifierDependency id']
    compilerResult $ CompilerRequire id' $ do
        result <- compilerUnsafeIO $ Store.get store (key id')
        case result of
            Store.NotFound      -> compilerThrow notFound
            Store.WrongType e r -> compilerThrow $ wrongType e r
            Store.Found x       -> return $ Item id' x
  where
    notFound =
        "Hakyll.Core.Compiler.Require.require: " ++ show id' ++ " was " ++
        "not found in the cache, the cache might be corrupted or " ++
        "the item you are referring to might not exist"
    wrongType e r =
        "Hakyll.Core.Compiler.Require.require: " ++ show id' ++ " was found " ++
        "in the cache, but does not have the right type: expected " ++ show e ++
        " but got " ++ show r


--------------------------------------------------------------------------------
requireBody :: (Binary a, Typeable a) => Identifier -> Compiler a
requireBody = fmap itemBody . require


--------------------------------------------------------------------------------
requireAll :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
requireAll pattern = do
    universe <- compilerUniverse <$> compilerAsk
    let matching = filterMatches pattern universe
    compilerTellDependencies [PatternDependency pattern matching]
    mapM require matching


--------------------------------------------------------------------------------
key :: Identifier -> [String]
key identifier = ["Hakyll.Core.Compiler.Require", show identifier]
