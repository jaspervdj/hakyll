--------------------------------------------------------------------------------
module Hakyll.Core.Compiler.Require
    ( Snapshot
    , save
    , saveSnapshot
    , require
    , requireSnapshot
    , requireBody
    , requireSnapshotBody
    , requireAll
    , requireAllSnapshots
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
import           Hakyll.Core.Metadata
import           Hakyll.Core.Store              (Store)
import qualified Hakyll.Core.Store              as Store


--------------------------------------------------------------------------------
type Snapshot = String


--------------------------------------------------------------------------------
save :: (Binary a, Typeable a) => Store -> Item a -> IO ()
save store item = saveSnapshot store final item


--------------------------------------------------------------------------------
saveSnapshot :: (Binary a, Typeable a)
             => Store -> Snapshot -> Item a -> IO ()
saveSnapshot store snapshot item =
    Store.set store (key (itemIdentifier item) snapshot) (itemBody item)


--------------------------------------------------------------------------------
require :: (Binary a, Typeable a) => Identifier -> Compiler (Item a)
require id' = requireSnapshot id' final


--------------------------------------------------------------------------------
requireSnapshot :: (Binary a, Typeable a)
                => Identifier -> Snapshot -> Compiler (Item a)
requireSnapshot id' snapshot = do
    store <- compilerStore <$> compilerAsk

    compilerTellDependencies [IdentifierDependency id']
    compilerResult $ CompilerRequire id' $ do
        result <- compilerUnsafeIO $ Store.get store (key id' snapshot)
        case result of
            Store.NotFound      -> compilerThrow notFound
            Store.WrongType e r -> compilerThrow $ wrongType e r
            Store.Found x       -> return $ Item id' x
  where
    notFound =
        "Hakyll.Core.Compiler.Require.require: " ++ show id' ++
        " (snapshot " ++ snapshot ++ ") was not found in the cache, " ++
        "the cache might be corrupted or " ++
        "the item you are referring to might not exist"
    wrongType e r =
        "Hakyll.Core.Compiler.Require.require: " ++ show id' ++
        " (snapshot " ++ snapshot ++ ") was found in the cache, " ++
        "but does not have the right type: expected " ++ show e ++
        " but got " ++ show r


--------------------------------------------------------------------------------
requireBody :: (Binary a, Typeable a) => Identifier -> Compiler a
requireBody id' = requireSnapshotBody id' final


--------------------------------------------------------------------------------
requireSnapshotBody :: (Binary a, Typeable a)
                    => Identifier -> Snapshot -> Compiler a
requireSnapshotBody id' snapshot = fmap itemBody $ requireSnapshot id' snapshot


--------------------------------------------------------------------------------
requireAll :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
requireAll pattern = requireAllSnapshots pattern final


--------------------------------------------------------------------------------
requireAllSnapshots :: (Binary a, Typeable a)
                    => Pattern -> Snapshot -> Compiler [Item a]
requireAllSnapshots pattern snapshot = do
    matching <- getMatches pattern
    mapM (\i -> requireSnapshot i snapshot) matching


--------------------------------------------------------------------------------
key :: Identifier -> String -> [String]
key identifier snapshot =
    ["Hakyll.Core.Compiler.Require", show identifier, snapshot]


--------------------------------------------------------------------------------
final :: Snapshot
final = "final"
