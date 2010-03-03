module Text.Hakyll.RenderAction
    ( RenderAction (..)
    , fromRenderable
    ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad ((<=<), mplus)

import Text.Hakyll.Hakyll
import Text.Hakyll.Context
import Text.Hakyll.Renderable

data RenderAction a b = RenderAction
    { actionDependencies :: [FilePath]
    , actionDestination  :: Maybe (Hakyll FilePath)
    , actionFunction     :: a -> Hakyll b
    }

instance Category RenderAction where
    id = RenderAction
        { actionDependencies = []
        , actionDestination  = Nothing
        , actionFunction     = return
        }

    x . y = RenderAction
        { actionDependencies = actionDependencies x ++ actionDependencies y
        , actionDestination  = actionDestination y `mplus` actionDestination x
        , actionFunction     = actionFunction x <=< actionFunction y
        }

fromRenderable :: (Renderable a)
               => a
               -> RenderAction () Context
fromRenderable renderable = RenderAction
    { actionDependencies = getDependencies renderable
    , actionDestination  = Just $ getUrl renderable
    , actionFunction     = const $ toContext renderable
    }
