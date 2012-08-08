-- | Read templates in Hakyll's native format
--
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Template.Read.Hakyll
    ( readTemplate
    ) where

import Data.List (isPrefixOf)
import Data.Char (isAlphaNum)

import Hakyll.Web.Template.Internal

-- | Construct a @Template@ from a string.
--
readTemplate :: TemplateString a => a -> Template a
readTemplate = Template . readTemplate'
  where
    readTemplate' string
        | tsNull string = []
        | "$$" `tsIsPrefixOf` string =
            Escaped : readTemplate' (tsDrop 2 string)
        | "$" `tsIsPrefixOf` string =
            case readKey (tsDrop 1 string) of
                Just (key, rest) -> Key (tsToString key) : readTemplate' rest
                Nothing          -> Chunk "$" : readTemplate' (tsDrop 1 string)
        | otherwise =
            let (chunk, rest) = tsBreak (== '$') string
            in Chunk chunk : readTemplate' rest

    -- Parse an key into (key, rest) if it's valid, and return
    -- Nothing otherwise
    readKey string =
        let (key, rest) = tsSpan isAlphaNum string
        in if not (tsNull key) && "$" `tsIsPrefixOf` rest
            then Just (key, tsDrop 1 rest)
            else Nothing
