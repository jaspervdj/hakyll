--------------------------------------------------------------------------------
module Hakyll.Core.Metadata
    ( Metadata
    , MonadMetadata (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                  (forM)
import           Data.Map                       (Map)


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern


--------------------------------------------------------------------------------
type Metadata = Map String String


--------------------------------------------------------------------------------
class Monad m => MonadMetadata m where
    getMetadata    :: Identifier -> m Metadata
    getMatches     :: Pattern -> m [Identifier]

    getAllMetadata :: Pattern -> m [(Identifier, Metadata)]
    getAllMetadata pattern = do
        matches' <- getMatches pattern
        forM matches' $ \id' -> do
            metadata <- getMetadata id'
            return (id', metadata)
