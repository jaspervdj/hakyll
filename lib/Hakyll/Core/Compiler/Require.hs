--------------------------------------------------------------------------------
module Hakyll.Core.Compiler.Require
    ( Snapshot
    , save
    , saveSnapshot
    , load
    , loadSnapshot
    , loadBody
    , loadSnapshotBody
    , loadAll
    , loadAllSnapshots
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                  (when)
import           Data.Binary                    (Binary)
import           Data.Foldable                  (toList, traverse_)
import           Data.Functor.Identity          (Identity(Identity, runIdentity))
import qualified Data.Set                       as S
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
save :: (Binary a, Typeable a) => Store -> Item a -> IO ()
save store item = saveSnapshot store final item


--------------------------------------------------------------------------------
-- | Save a specific snapshot of an item, so you can load it later using
-- 'loadSnapshot'.
saveSnapshot :: (Binary a, Typeable a)
             => Store -> Snapshot -> Item a -> IO ()
saveSnapshot store snapshot item =
    Store.set store (key (itemIdentifier item) snapshot) (itemBody item)


--------------------------------------------------------------------------------
-- | Load an item compiled elsewhere. If the required item is not yet compiled,
-- the build system will take care of that automatically.
load :: (Binary a, Typeable a) => Identifier -> Compiler (Item a)
load id' = loadSnapshot id' final


--------------------------------------------------------------------------------
-- | Require a specific snapshot of an item.
loadSnapshot :: (Binary a, Typeable a)
             => Identifier -> Snapshot -> Compiler (Item a)
loadSnapshot id' snapshot =
    fmap runIdentity $ loadSnapshotCollection (Identity (id', snapshot))


--------------------------------------------------------------------------------
-- | A shortcut for only requiring the body of an item.
--
-- > loadBody = fmap itemBody . load
loadBody :: (Binary a, Typeable a) => Identifier -> Compiler a
loadBody id' = loadSnapshotBody id' final


--------------------------------------------------------------------------------
-- | A shortcut for only requiring the body for a specific snapshot of an item
loadSnapshotBody :: (Binary a, Typeable a)
                 => Identifier -> Snapshot -> Compiler a
loadSnapshotBody id' snapshot = fmap itemBody $ loadSnapshot id' snapshot


--------------------------------------------------------------------------------
-- | This function allows you to 'load' a dynamic list of items
loadAll :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadAll pattern = loadAllSnapshots pattern final


--------------------------------------------------------------------------------
-- | Load a specific snapshot for each of dynamic list of items
loadAllSnapshots :: (Binary a, Typeable a)
                 => Pattern -> Snapshot -> Compiler [Item a]
loadAllSnapshots pattern snapshot = do
    ids <- fmap (\id' -> (id', snapshot)) <$> getMatches pattern
    loadSnapshotCollection ids


--------------------------------------------------------------------------------
-- | Load a collection of snapshots.
-- Only the first NotFound or WrongType error will be reported.
loadSnapshotCollection :: (Binary a, Typeable a, Traversable t)
              => t (Identifier, Snapshot) -> Compiler (t (Item a))
loadSnapshotCollection ids = do
    store    <- compilerStore <$> compilerAsk
    universe <- compilerUniverse <$> compilerAsk

    -- Quick check for better error messages
    let checkMember (id', snap) =
            when (id' `S.notMember` universe) (fail $ notFound id' snap)
    traverse_ checkMember ids

    compilerTellDependencies $ IdentifierDependency . fst <$> toList ids
    let go (id', snap) = do
            result <- compilerUnsafeIO $ Store.get store (key id' snap)
            case result of
                Store.NotFound      -> fail $ notFound id' snap
                Store.WrongType e r -> fail $ wrongType id' snap e r
                Store.Found x       -> return $ Item id' x
    compilerResult $ CompilerRequire (toList ids) $ traverse go ids
  where
    notFound id' snapshot =
        "Hakyll.Core.Compiler.Require.load: " ++ show id' ++
        " (snapshot " ++ snapshot ++ ") was not found in the cache, " ++
        "the cache might be corrupted or " ++
        "the item you are referring to might not exist"
    wrongType id' snapshot e r =
        "Hakyll.Core.Compiler.Require.load: " ++ show id' ++
        " (snapshot " ++ snapshot ++ ") was found in the cache, " ++
        "but does not have the right type: expected " ++ show e ++
        " but got " ++ show r


--------------------------------------------------------------------------------
key :: Identifier -> String -> [String]
key identifier snapshot =
    ["Hakyll.Core.Compiler.Require", show identifier, snapshot]


--------------------------------------------------------------------------------
final :: Snapshot
final = "_final"
