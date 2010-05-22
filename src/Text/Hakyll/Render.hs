-- | Module containing rendering functions. All these functions are used to
--   render files to the @_site@ directory.
module Text.Hakyll.Render 
    ( render
    , renderAndConcat
    , renderChain
    , static
    , css
    , writePage
    ) where

import Control.Arrow ((>>>))
import Control.Applicative ((<$>))
import Control.Monad.Reader (liftIO)
import System.Directory (copyFile)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Text.Hakyll.Context (Context)
import Text.Hakyll.HakyllMonad (Hakyll, askHakyll, getAdditionalContext)
import Text.Hakyll.File
import Text.Hakyll.HakyllAction
import Text.Hakyll.Internal.CompressCss
import Text.Hakyll.Internal.Template

-- | A pure render function - used internally.
pureRender :: Template -- ^ Template to use for rendering.
           -> Context  -- ^ Renderable object to render with given template.
           -> Context  -- ^ The body of the result will contain the render.
pureRender template context =
    -- Ignore $root when substituting here. We will only replace that in the
    -- final render (just before writing).
    let contextIgnoringRoot = M.insert "root" "$root" context
        body = regularSubstitute template contextIgnoringRoot
    in M.insert "body" body context

-- | This is the most simple render action. You render a @Context@ with a
--   template, and get back the result.
render :: FilePath                     -- ^ Template to use for rendering.
       -> HakyllAction Context Context -- ^ The render computation.
render templatePath = HakyllAction
    { actionDependencies = [templatePath]
    , actionUrl          = Right id
    , actionFunction     = \context ->
        flip pureRender context <$> readTemplate templatePath
    }

-- | Render each @Context@ with the given templates, then concatenate the
--   result. So, basically this function:
--
--   - Takes every @Context@.
--
--   - Renders every @Context@ with all given templates. This is comparable
--     with a renderChain action.
--
--   - Concatenates the result and returns that as a @String@.
renderAndConcat :: [FilePath]
                -> [HakyllAction () Context]
                -> HakyllAction () String
renderAndConcat templatePaths renderables = HakyllAction
    { actionDependencies = renders >>= actionDependencies
    , actionUrl          = Right id
    , actionFunction     = actionFunction'
    }
  where
    render' = chain (map render templatePaths)
    renders = map (>>> render') renderables

    actionFunction' _ = do
        contexts <- mapM runHakyllAction renders
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
renderChain :: [FilePath]
            -> HakyllAction () Context
            -> Hakyll ()
renderChain templatePaths initial =
    runHakyllActionIfNeeded renderChainWith'
  where
    renderChainWith' = initial >>> chain' >>> writePage
    chain' = chain $ map render templatePaths

-- | Mark a certain file as static, so it will just be copied when the site is
--   generated.
static :: FilePath -> Hakyll ()
static source = runHakyllActionIfNeeded static'
  where
    static' = createFileHakyllAction source $ do
        destination <- toDestination source
        makeDirectories destination
        liftIO $ copyFile source destination

-- | Render a css file, compressing it.
css :: FilePath -> Hakyll ()
css source = runHakyllActionIfNeeded css'
  where
    css' = createFileHakyllAction source $ do
        contents <- liftIO $ readFile source
        destination <- toDestination source
        makeDirectories destination
        liftIO $ writeFile destination (compressCss contents)

-- | Write a page to the site destination. Final action after render
--   chains and such.
writePage :: HakyllAction Context ()
writePage = createHakyllAction $ \initialContext -> do
    additionalContext' <- askHakyll getAdditionalContext
    let url = fromMaybe (error "No url defined at write time.")
                        (M.lookup "url" initialContext)
        body = fromMaybe "" (M.lookup "body" initialContext)
    let context = additionalContext' `M.union` M.singleton "root" (toRoot url)
    destination <- toDestination url
    makeDirectories destination
    -- Substitute $root here, just before writing.
    liftIO $ writeFile destination $ finalSubstitute (fromString body) context
