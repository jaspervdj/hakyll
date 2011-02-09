module Hakyll.Web.Template
    ( Template
    , readTemplate
    , applyTemplate
    , applySelf
    ) where

import Data.List (isPrefixOf)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Hakyll.Web.Template.Internal
import Hakyll.Web.Page

-- | Construct a @Template@ from a string.
--
readTemplate :: String -> Template
readTemplate = Template . readTemplate'
  where
    readTemplate' [] = []
    readTemplate' string
        | "$$" `isPrefixOf` string =
            Escaped : readTemplate' (drop 2 string)
        | "$" `isPrefixOf` string =
            case readIdentifier (drop 1 string) of
                Just (key, rest) -> Identifier key : readTemplate' rest
                Nothing          -> Chunk "$" : readTemplate' (drop 1 string)
        | otherwise =
            let (chunk, rest) = break (== '$') string
            in Chunk chunk : readTemplate' rest

    -- Parse an identifier into (identifier, rest) if it's valid, and return
    -- Nothing otherwise
    readIdentifier string =
        let (identifier, rest) = span isAlphaNum string
        in if not (null identifier) && "$" `isPrefixOf` rest
            then Just (identifier, drop 1 rest)
            else Nothing

-- | Substitutes @$identifiers@ in the given @Template@ by values from the given
--   "Page". When a key is not found, it is left as it is. You can specify
--   the characters used to replace escaped dollars (@$$@) here.
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
