-- | Internal module do some low-level rendering.
module Text.Hakyll.Render.Internal
    ( substitute
    , regularSubstitute
    , finalSubstitute
    , pureRenderWith
    , writePage
    ) where

import qualified Data.Map as M
import Text.Hakyll.Context (Context, ContextManipulation)
import Data.List (isPrefixOf)
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import Control.Parallel.Strategies (rnf, ($|))
import Text.Hakyll.Renderable
import Text.Hakyll.Page
import Text.Hakyll.File

-- | Substitutes `$identifiers` in the given string by values from the given
--   "Context". When a key is not found, it is left as it is. You can here
--   specify the characters used to replace escaped dollars `$$`.
substitute :: String -> String -> Context -> String 
substitute _ [] _ = []
substitute escaper string context 
    | "$$" `isPrefixOf` string = escaper ++ substitute' (tail tail')
    | "$" `isPrefixOf` string = substituteKey
    | otherwise = (head string) : (substitute' tail')
  where
    tail' = tail string
    (key, rest) = break (not . isAlpha) tail'
    replacement = fromMaybe ('$' : key) $ M.lookup key context
    substituteKey = replacement ++ substitute' rest
    substitute' str = substitute escaper str context

-- | "substitute" for use during a chain.
regularSubstitute :: String -> Context -> String
regularSubstitute = substitute "$$"

-- | "substitute" for the end of a chain (just before writing).
finalSubstitute :: String -> Context -> String
finalSubstitute = substitute "$"

-- | A pure render function.
pureRenderWith :: ContextManipulation -- ^ Manipulation to apply on the context.
               -> String -- ^ Template to use for rendering.
               -> Context -- ^ Renderable object to render with given template.
               -> Page -- ^ The body of the result will contain the render.
pureRenderWith manipulation template context =
    -- Ignore $root when substituting here. We will only replace that in the
    -- final render (just before writing).
    let contextIgnoringRoot = M.insert "root" "$root" (manipulation context)
        body = regularSubstitute template contextIgnoringRoot
    -- Force the body to be rendered.
    in ($|) fromContext rnf (M.insert "body" body context)

-- | Write a page to the site destination. Final action after render
--   chains and such.
writePage :: Page -> IO ()
writePage page = do
    let destination = toDestination url
    makeDirectories destination
    writeFile destination body
  where
    url = getURL page
    -- Substitute $root here, just before writing.
    body = finalSubstitute (getBody page)
                           (M.singleton "root" $ toRoot url)
