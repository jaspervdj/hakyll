-- | Internal module do some low-level rendering.
module Text.Hakyll.Render.Internal
    ( substitute
    , regularSubstitute
    , finalSubstitute
    , pureRenderWith
    , pureRenderAndConcatWith
    , pureRenderChainWith
    , writePage
    ) where

import qualified Data.Map as M
import Text.Hakyll.Context (Context, ContextManipulation)
import Control.Monad.Reader (liftIO)
import Data.List (isPrefixOf, foldl')
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import Control.Parallel.Strategies (rdeepseq, ($|))

import Text.Hakyll.Renderable
import Text.Hakyll.Page
import Text.Hakyll.File
import Text.Hakyll.Hakyll

-- | Substitutes @$identifiers@ in the given string by values from the given
--   "Context". When a key is not found, it is left as it is. You can here
--   specify the characters used to replace escaped dollars (@$$@).
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
               -> Context -- ^ The body of the result will contain the render.
pureRenderWith manipulation template context =
    -- Ignore $root when substituting here. We will only replace that in the
    -- final render (just before writing).
    let contextIgnoringRoot = M.insert "root" "$root" (manipulation context)
        body = regularSubstitute template contextIgnoringRoot
    -- Force the body to be rendered.
    in ($|) id rdeepseq (M.insert "body" body context)

-- | A pure renderAndConcat function.
pureRenderAndConcatWith :: ContextManipulation
                        -> [String] -- ^ Templates to use.
                        -> [Context] -- ^ Different renderables.
                        -> String
pureRenderAndConcatWith manipulation templates contexts =
    foldl' renderAndConcat [] contexts
  where
    renderAndConcat chunk context =
        let rendered = pureRenderChainWith manipulation templates context
        in chunk ++ fromMaybe "" (M.lookup "body" rendered)

-- | A pure renderChain function.
pureRenderChainWith :: ContextManipulation
                    -> [String]
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
    let destination = toDestination url
        context = additionalContext' `M.union` (M.singleton "root" $ toRoot url)
    makeDirectories destination
    -- Substitute $root here, just before writing.
    liftIO $ writeFile destination $ finalSubstitute (getBody page) context
  where
    url = getURL page
