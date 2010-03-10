-- | Internal module do some low-level rendering.
module Text.Hakyll.Internal.Render
    ( pureRender
    , writePage
    ) where

import qualified Data.Map as M
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe)

import Text.Hakyll.File
import Text.Hakyll.Hakyll
import Text.Hakyll.HakyllAction
import Text.Hakyll.Internal.Template

-- | A pure render function.
pureRender :: Template -- ^ Template to use for rendering.
           -> Context -- ^ Renderable object to render with given template.
           -> Context -- ^ The body of the result will contain the render.
pureRender template context =
    -- Ignore $root when substituting here. We will only replace that in the
    -- final render (just before writing).
    let contextIgnoringRoot = M.insert "root" "$root" context
        body = regularSubstitute template contextIgnoringRoot
    in M.insert "body" body context

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
