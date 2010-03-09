-- | This is the module which exports @RenderAction@.
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

import Control.Arrow
import Control.Category
import Control.Monad ((<=<), mplus, unless)
import Control.Monad.Reader (liftIO)
import Prelude hiding ((.), id)
import System.IO (hPutStrLn, stderr)

import Text.Hakyll.Context
import Text.Hakyll.File (toDestination, isFileMoreRecent)
import Text.Hakyll.Hakyll

-- | Type used for rendering computations that carry along dependencies.
data RenderAction a b = RenderAction
    { -- | Dependencies of the @RenderAction@.
      actionDependencies :: [FilePath]
    , -- | URL pointing to the result of this @RenderAction@.
      actionUrl          :: Maybe (Hakyll FilePath)
    , -- | The actual render function.
      actionFunction     :: a -> Hakyll b
    }

-- | Create a @RenderAction@ from a function.
createRenderAction :: (a -> Hakyll b)  -- ^ Function to execute.
                   -> RenderAction a b
createRenderAction f = id { actionFunction = f }

-- | Create a @RenderAction@ from a simple @Hakyll@ value.
createSimpleRenderAction :: Hakyll b -- ^ Hakyll value to pass on.
                         -> RenderAction () b
createSimpleRenderAction = createRenderAction . const

-- | Create a @RenderAction@ that operates on one file.
createFileRenderAction :: FilePath          -- ^ File to operate on.
                       -> Hakyll b          -- ^ Value to pass on.
                       -> RenderAction () b -- ^ The resulting action.
createFileRenderAction path action = RenderAction
    { actionDependencies = [path]
    , actionUrl          = Just $ return path
    , actionFunction     = const action
    }

-- | Create a @RenderAction@ from a @ContextManipulation@.
createManipulationAction :: ContextManipulation -- ^ Manipulation to apply.
                         -> RenderAction Context Context
createManipulationAction = createRenderAction . (return .)

-- | Run a @RenderAction@ now.
runRenderAction :: RenderAction () a -- ^ Render action to run.
                -> Hakyll a          -- ^ Result of the action.
runRenderAction action = actionFunction action ()

-- | Run a @RenderAction@, but only when it is out-of-date. At this point, the
--   @actionUrl@ field must be set.
runRenderActionIfNeeded :: RenderAction () () -- ^ Action to run.
                        -> Hakyll ()          -- ^ Empty result.
runRenderActionIfNeeded action = do
    url <- case actionUrl action of
        (Just u) -> u
        Nothing  -> error "No url when checking dependencies."
    destination <- toDestination url
    valid <- isFileMoreRecent destination $ actionDependencies action
    unless valid $ do liftIO $ hPutStrLn stderr $ "Rendering " ++ destination
                      runRenderAction action

-- | Chain a number of @RenderAction@ computations.
chain :: [RenderAction a a] -- ^ Actions to chain.
      -> RenderAction a a   -- ^ Resulting action.
chain []         = id
chain list@(_:_) = foldl1 (>>>) list

-- | This is a specialized version of @RenderAction@, a @Context@ that can be
--   rendered.
type Renderable = RenderAction () Context

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

instance Arrow RenderAction where
    arr f = id { actionFunction = \x -> return (f x) }

    first x = RenderAction
        { actionDependencies = actionDependencies x
        , actionUrl          = actionUrl x
        , actionFunction     = \(y, z) -> do y' <- (actionFunction x) y
                                             return (y', z)
        }
