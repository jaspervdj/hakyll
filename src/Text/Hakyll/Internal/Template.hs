module Text.Hakyll.Internal.Template
    ( Template (..)
    , fromString
    , readTemplate
    , substitute
    , regularSubstitute
    , finalSubstitute
    ) where

import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import qualified Data.Map as M

import Text.Hakyll.Context (Context (..))
import Text.Hakyll.HakyllMonad (Hakyll)
import Text.Hakyll.Internal.Cache
import Text.Hakyll.Internal.Page
import Text.Hakyll.Internal.Template.Template
import Text.Hakyll.Internal.Template.Hamlet

-- | Construct a @Template@ from a string.
--
fromString :: String -> Template
fromString = Template . fromString'
  where
    fromString' [] = []
    fromString' string
        | "$$" `isPrefixOf` string =
            EscapeCharacter : (fromString' $ tail tail')
        | "$" `isPrefixOf` string =
            let (key, rest) = span isAlphaNum tail'
            in Identifier key : fromString' rest
        | otherwise =
            let (chunk, rest) = break (== '$') string
            in Chunk chunk : fromString' rest
      where
        tail' = tail string

-- | Read a @Template@ from a file. This function might fetch the @Template@
--   from the cache, if available.
readTemplate :: FilePath -> Hakyll Template
readTemplate path = do
    isCacheMoreRecent' <- isCacheMoreRecent fileName [path]
    if isCacheMoreRecent'
        then getFromCache fileName
        else do
            template <- if isHamletRTFile path
                            then readHamletTemplate
                            else readDefaultTemplate
            storeInCache template fileName
            return template
  where 
    fileName = "templates" </> path
    readDefaultTemplate = do
        page <- unContext <$> readPage path
        let body = fromMaybe (error $ "No body in template " ++ fileName)
                             (M.lookup "body" page)
        return $ fromString body

    readHamletTemplate = fromHamletRT <$> readHamletRT path

-- | Substitutes @$identifiers@ in the given @Template@ by values from the given
--   "Context". When a key is not found, it is left as it is. You can specify
--   the characters used to replace escaped dollars (@$$@) here.
substitute :: String -> Template -> Context -> String 
substitute escaper template context = substitute' =<< unTemplate template
  where
    substitute' (Chunk chunk) = chunk
    substitute' (Identifier key) =
        fromMaybe ('$' : key) $ M.lookup key $ unContext context
    substitute' (EscapeCharacter) = escaper

-- | @substitute@ for use during a chain. This will leave escaped characters as
--   they are.
regularSubstitute :: Template -> Context -> String
regularSubstitute = substitute "$$"

-- | @substitute@ for the end of a chain (just before writing). This renders
--   escaped characters.
finalSubstitute :: Template -> Context -> String
finalSubstitute = substitute "$"
