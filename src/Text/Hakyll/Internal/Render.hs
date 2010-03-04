-- | Internal module do some low-level rendering.
module Text.Hakyll.Internal.Render
    ( substitute
    , regularSubstitute
    , finalSubstitute
    , pureRenderWith
    , pureRenderAndConcatWith
    , pureRenderChainWith
    , writePage
    ) where

import qualified Data.Map as M
import Control.Monad.Reader (liftIO)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Text.Hakyll.Context (Context, ContextManipulation)
import Text.Hakyll.Renderable
import Text.Hakyll.File
import Text.Hakyll.Hakyll
import Text.Hakyll.Internal.Page
import Text.Hakyll.Internal.Template

-- | A pure render function.
pureRenderWith :: ContextManipulation -- ^ Manipulation to apply on the context.
               -> Template -- ^ Template to use for rendering.
               -> Context -- ^ Renderable object to render with given template.
               -> Context -- ^ The body of the result will contain the render.
pureRenderWith manipulation template context =
    -- Ignore $root when substituting here. We will only replace that in the
    -- final render (just before writing).
    let contextIgnoringRoot = M.insert "root" "$root" (manipulation context)
        body = regularSubstitute template contextIgnoringRoot
    in M.insert "body" body context

-- | A pure renderAndConcat function.
pureRenderAndConcatWith :: ContextManipulation -- ^ Manipulation to apply.
                        -> [Template] -- ^ Templates to use.
                        -> [Context] -- ^ Different renderables.
                        -> String
pureRenderAndConcatWith manipulation templates =
    concatMap renderAndConcat
  where
    renderAndConcat = fromMaybe "" . M.lookup "body"
                    . pureRenderChainWith manipulation templates

-- | A pure renderChain function.
pureRenderChainWith :: ContextManipulation
                    -> [Template]
                    -> Context
                    -> Context
pureRenderChainWith manipulation templates context =
    let initial = manipulation context
    in foldl' (flip $ pureRenderWith id) initial templates

-- | Write a page to the site destination. Final action after render
--   chains and such.
writePage :: Page -> Hakyll ()
writePage page = do
    additionalContext' <- askHakyll additionalContext
    url <- getUrl page
    destination <- toDestination url
    let context = additionalContext' `M.union` M.singleton "root" (toRoot url)
    makeDirectories destination
    -- Substitute $root here, just before writing.
    liftIO $ writeFile destination $ finalSubstitute (fromString $ getBody page)
                                                     context
