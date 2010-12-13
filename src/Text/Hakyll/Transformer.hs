-- | This is the module which exports @Transformer@.
module Text.Hakyll.Transformer
    ( Transformer (..)
    , transformResource
    , transformResourceM
    , transformData
    , transformDataM
    , transformMetaData
    , transformMetaDataM
    , runTransformer
    , runTransformerForced
    ) where

import Data.Monoid (mappend, mempty)
import Control.Arrow
import Control.Category
import Control.Applicative ((<$>))
import Control.Monad ((<=<), unless)
import Prelude hiding ((.), id)

import Text.Hakyll.Resource
import Text.Hakyll.File (toDestination, isFileMoreRecent)
import Text.Hakyll.Monad

-- | Type used for computations that transform resources, carrying along
-- dependencies.
--
data Transformer a b = Transformer
    { -- | Dependencies of the @Transformer@.
      transformerDependencies :: [FilePath]
    , -- | URL pointing to the result of this @Transformer@.
      transformerUrl          :: FilePath -> Hakyll FilePath
    , -- | The actual transforming function.
      transformerFunction     :: Resource a -> Hakyll (Resource b)
    }

instance Category Transformer where
    id = Transformer
        { transformerDependencies = []
        , transformerUrl          = return
        , transformerFunction     = return
        }

    x . y = Transformer
        { transformerDependencies =
            transformerDependencies x ++ transformerDependencies y
        , transformerUrl = transformerUrl y <=< transformerUrl x
        , transformerFunction = transformerFunction x <=< transformerFunction y
        }

instance Arrow Transformer where
    arr = transformData

    first t = t
        { transformerFunction = \(Resource m (x, y)) -> do
            Resource m' x' <- transformerFunction t $ Resource m x
            return $ Resource (mappend m' m) (x', y)
        }

transformResource :: (Resource a -> Resource b) -> Transformer a b
transformResource = transformResourceM . (return .)

transformResourceM :: (Resource a -> Hakyll (Resource b)) -> Transformer a b
transformResourceM f = id {transformerFunction = f}

transformData :: (a -> b) -> Transformer a b
transformData = transformResource . fmap

transformDataM :: (a -> Hakyll b) -> Transformer a b
transformDataM f = transformResourceM $ \(Resource m x) ->
    f x >>= return . Resource m

transformMetaData :: (Metadata -> Metadata) -> Transformer a a
transformMetaData = transformMetaDataM . (return .)

transformMetaDataM :: (Metadata -> Hakyll Metadata) -> Transformer a a
transformMetaDataM f = transformResourceM $ \(Resource m x) -> do
    m' <- f m
    return $ Resource m' x

-- | Run a transformer. This might not run it when the result is up-to-date
--
runTransformer :: Transformer () ()
               -> Hakyll ()
runTransformer t = do
    url <- transformerUrl t $
        error "runTransformer: No url when checking dependencies."
    destination <- toDestination url
    valid <- isFileMoreRecent destination $ transformerDependencies t
    unless valid $ do logHakyll $ "Rendering " ++ destination
                      runTransformerForced t

-- | Always run the transformer, even when the target is up-to-date
--
runTransformerForced :: Transformer () ()
                     -> Hakyll ()
runTransformerForced t = getData <$> transformerFunction t mempty
