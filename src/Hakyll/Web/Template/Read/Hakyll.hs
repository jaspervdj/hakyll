-- | Read templates in Hakyll's native format
--
module Hakyll.Web.Template.Read.Hakyll
    ( readTemplate
    ) where

import Data.List (isPrefixOf)
import Data.Char (isAlphaNum)

import Hakyll.Web.Template.Internal

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
