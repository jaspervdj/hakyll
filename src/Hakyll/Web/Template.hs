module Hakyll.Web.Template
    ( Template
    , applyTemplate
    , applySelf
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Hakyll.Web.Template.Internal
import Hakyll.Web.Template.Read.Hakyll (readTemplate)
import Hakyll.Web.Page

-- | Substitutes @$identifiers@ in the given @Template@ by values from the given
-- "Page". When a key is not found, it is left as it is. You can specify
-- the characters used to replace escaped dollars (@$$@) here.
--
applyTemplate :: Template -> Page String -> Page String
applyTemplate template page =
    fmap (const $ substitute =<< unTemplate template) page
  where
    substitute (Chunk chunk) = chunk
    substitute (Identifier key) =
        fromMaybe ('$' : key) $ M.lookup key $ toMap page
    substitute (Escaped) = "$"

-- | Apply a page as it's own template. This is often very useful to fill in
-- certain keys like @$root@ and @$url@.
--
applySelf :: Page String -> Page String
applySelf page = applyTemplate (readTemplate $ pageBody page) page
