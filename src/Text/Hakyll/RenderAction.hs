module Text.Hakyll.RenderAction
    ( RenderAction (..)
    , createRenderAction
    , createSimpleRenderAction
    , createManipulationAction
    , chain
    , runRenderAction
    ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad ((<=<), mplus)

import Text.Hakyll.Hakyll
import Text.Hakyll.Context

data RenderAction a b = RenderAction
    { actionDependencies :: [FilePath]
    , actionUrl          :: Maybe (Hakyll FilePath)
    , actionFunction     :: a -> Hakyll b
    }

createRenderAction :: (a -> Hakyll b) -> RenderAction a b
createRenderAction f = id { actionFunction = f }

createSimpleRenderAction :: Hakyll b -> RenderAction () b
createSimpleRenderAction = createRenderAction . const

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
