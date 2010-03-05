module Text.Hakyll.RenderAction
    ( RenderAction (..)
    , createRenderAction
    , createSimpleRenderAction
    , createFileRenderAction
    , createManipulationAction
    , chain
    , runRenderAction
    , runRenderActionIfNeeded
    , Renderable
    ) where

import Control.Category
import Control.Monad ((<=<), mplus, unless)
import Control.Monad.Reader (liftIO)
import Prelude hiding ((.), id)
import System.IO (hPutStrLn, stderr)

import Text.Hakyll.Context
import Text.Hakyll.File (toDestination, isFileMoreRecent)
import Text.Hakyll.Hakyll

data RenderAction a b = RenderAction
    { actionDependencies :: [FilePath]
    , actionUrl          :: Maybe (Hakyll FilePath)
    , actionFunction     :: a -> Hakyll b
    }

createRenderAction :: (a -> Hakyll b) -> RenderAction a b
createRenderAction f = id { actionFunction = f }

createSimpleRenderAction :: Hakyll b -> RenderAction () b
createSimpleRenderAction = createRenderAction . const

createFileRenderAction :: FilePath -> Hakyll b -> RenderAction () b
createFileRenderAction path action = RenderAction
    { actionDependencies = [path]
    , actionUrl          = Just $ return path
    , actionFunction     = const action
    }

instance Category RenderAction where
    id = RenderAction
        { actionDependencies = []
        , actionUrl          = Nothing
        , actionFunction     = return
        }

    x . y = RenderAction
        { actionDependencies = actionDependencies x ++ actionDependencies y
        , actionUrl          = actionUrl y `mplus` actionUrl x
        , actionFunction     = actionFunction x <=< actionFunction y
        }

createManipulationAction :: ContextManipulation -> RenderAction Context Context
createManipulationAction = createRenderAction . (return .)

chain :: [RenderAction a a] -> RenderAction a a
chain = foldl1 (>>>)

runRenderAction :: RenderAction () a -> Hakyll a
runRenderAction action = actionFunction action ()

runRenderActionIfNeeded :: RenderAction () () -> Hakyll ()
runRenderActionIfNeeded action = do
    url <- case actionUrl action of
        (Just u) -> u
        Nothing  -> error "No url when checking dependencies."
    destination <- toDestination url
    valid <- isFileMoreRecent destination $ actionDependencies action
    unless valid $ do liftIO $ hPutStrLn stderr $ "Rendering " ++ destination
                      runRenderAction action

type Renderable = RenderAction () Context
