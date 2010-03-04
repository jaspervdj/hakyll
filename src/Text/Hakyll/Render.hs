-- | Module containing rendering functions. All these functions are used to
--   render files to the @_site@ directory.
module Text.Hakyll.Render 
    ( depends
    , render
    , renderWith
    , renderAndConcat
    , renderAndConcatWith
    , renderChain
    , renderChainWith
    , static
    , css
    ) where

import Control.Monad (unless)
import Control.Arrow ((>>>))
import Control.Monad.Reader (liftIO)
import System.Directory (copyFile)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Text.Hakyll.Hakyll (Hakyll)
import Text.Hakyll.Context (ContextManipulation, Context)
import Text.Hakyll.File
import Text.Hakyll.RenderAction
import Text.Hakyll.Internal.CompressCss
import Text.Hakyll.Internal.Render
import Text.Hakyll.Internal.Template (readTemplate)

-- | Execute an IO action only when the cache is invalid.
depends :: FilePath   -- ^ File to be rendered or created.
        -> [FilePath] -- ^ Files the render depends on.
        -> Hakyll ()  -- ^ Action to execute when the file is out of date.
        -> Hakyll ()
depends file dependencies action = do
    destination <- toDestination file
    valid <- isFileMoreRecent destination dependencies
    unless valid action

-- | Render to a Page.
render :: FilePath                     -- ^ Template to use for rendering.
       -> RenderAction Context Context -- ^ The render computation.
render = renderWith id

-- | Render to a Page. This function allows you to manipulate the context
--   first.
renderWith :: ContextManipulation          -- ^ Manipulation to apply first.
           -> FilePath                     -- ^ Template to use for rendering.
           -> RenderAction Context Context -- ^ The render computation.
renderWith manipulation templatePath = RenderAction
    { actionDependencies = [templatePath]
    , actionUrl          = Nothing
    , actionFunction     = actionFunction'
    }
  where
    actionFunction' context = do
        template <- readTemplate templatePath
        return $ pureRenderWith manipulation template context

-- | Render each renderable with the given templates, then concatenate the
--   result. So, basically this function:
--
--   * Takes every renderable.
--
--   * Renders every renderable with all given templates. This is comparable
--     with a renderChain action.
--
--   * Concatenates the result.
--
renderAndConcat :: [FilePath] -- ^ Templates to apply on every renderable.
                -> [RenderAction () Context] -- ^ Renderables to render.
                -> RenderAction () String
renderAndConcat = renderAndConcatWith id

-- | Render each renderable with the given templates, then concatenate the
--   result. This function allows you to specify a @ContextManipulation@ to
--   apply on every @Renderable@.
renderAndConcatWith :: ContextManipulation
                    -> [FilePath]
                    -> [RenderAction () Context]
                    -> RenderAction () String
renderAndConcatWith manipulation templatePaths renderables = RenderAction
    { actionDependencies = renders >>= actionDependencies
    , actionUrl           = Nothing
    , actionFunction     = actionFunction'
    }
  where
    render' = chain (map render templatePaths)
    renders = map (>>> manipulationAction >>> render') renderables
    manipulationAction = createManipulationAction manipulation

    actionFunction' _ = do
        contexts <- mapM runRenderAction renders
        return $ concatMap (fromMaybe "" . M.lookup "body") contexts

-- | Chain a render action for a page with a number of templates. This will
--   also write the result to the site destination. This is the preferred way
--   to do general rendering.
--
--   > renderChain [ "templates/notice.html"
--   >             , "templates/default.html"
--   >             ] $ createPagePath "warning.html"
--
--   This code will first render @warning.html@ using @templates/notice.html@,
--   and will then render the result with @templates/default.html@.
renderChain :: [FilePath] -> RenderAction () Context -> Hakyll ()
renderChain = renderChainWith id

-- | A more custom render chain that allows you to specify a
--   @ContextManipulation@ which to apply on the context when it is read first.
renderChainWith :: ContextManipulation
                -> [FilePath]
                -> RenderAction () Context
                -> Hakyll ()
renderChainWith manipulation templatePaths initial =
    runRenderAction renderChainWith'
  where
    renderChainWith' :: RenderAction () ()
    renderChainWith' = initial >>> manipulationAction >>> chain' >>> writePage

    chain' = chain (map render templatePaths)
    manipulationAction = createManipulationAction manipulation


-- | Mark a certain file as static, so it will just be copied when the site is
--   generated.
static :: FilePath -> Hakyll ()
static source = do destination <- toDestination source
                   depends destination [source] (action destination)
  where
    action destination = do makeDirectories destination
                            liftIO $ copyFile source destination

-- | Render a css file, compressing it.
css :: FilePath -> Hakyll ()
css source = do destination <- toDestination source
                depends destination [source] (css' destination)
  where
    css' destination = do contents <- liftIO $ readFile source
                          makeDirectories destination
                          liftIO $ writeFile destination (compressCss contents)
