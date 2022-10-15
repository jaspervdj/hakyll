{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------
-- | This module exposes a function to compress the HTML output.
--
-- The compression is very basic, shaving off about 1-3% of a typical HTML output,
-- and it works as follows:
--
-- * Comments are removed.
-- * Several consecutive whitespaces are replaced by a single one, unless within a <pre> tag.
-- * Within a <pre> tag, @n@ consecutive whitespaces are replaced by a single @\t@ character.
--   This is useful if a page is heavy on code listings.
--   Don't forget to add @tab-size: n@ to your CSS!
--
-- Any of these steps can be disabled, see 'CompressHtmlOpts'.

module Hakyll.Web.Html.Compress
  ( CompressHtmlOpts(..)
  , def
  , compressHtml
  , compressHtmlCompiler
  ) where

import           Data.Char
import           Data.Default
import qualified Data.Set as S
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Html
import           Text.HTML.TagSoup

-- | The configuration for the HTML compression.
data CompressHtmlOpts = CompressHtmlOpts
  { choRemoveComments :: Bool          -- ^ Whether to remove comments.
  , choCompressWhitespace :: Bool      -- ^ Whether to remove excessive whitespaces.
  , choTabSize :: Maybe Int            -- ^ Replace this many spaces in <pre> with @\t@ (if 'choCompressWhitespace' is set).
  }

instance Default CompressHtmlOpts where
  def = CompressHtmlOpts
          { choRemoveComments = True
          , choCompressWhitespace = True
          , choTabSize = Nothing
          }

-- | Compiler form of 'compressHtml'.
compressHtmlCompiler :: CompressHtmlOpts -> Item String -> Compiler (Item String)
compressHtmlCompiler opts item = pure $ compressHtml opts <$> item

-- | Compresses an HTML string according to the given configuration.
compressHtml :: CompressHtmlOpts -> String -> String
compressHtml CompressHtmlOpts{ .. } = withTagList go
  where
    go = foldr (.) id
        $ [ f
          | (True, f) <- [ (choRemoveComments, removeComments)
                         , (choCompressWhitespace, compressWS choTabSize)
                         ]
          ]

removeComments :: [Tag String] -> [Tag String]
removeComments = filter (not . isTagComment)

compressWS :: Maybe Int -> [Tag String] -> [Tag String]
compressWS maybeTabSize = go mempty
  where
    go stack =
      \case [] -> []
            (tag@(TagClose n) : rest) -> tag : go (S.delete n stack) rest
            (tag@(TagOpen n _) : rest) -> tag : go (S.insert n stack) rest
            (tag@(TagText text) : rest)
              -- all spaces within a <pre> are important, but we can replace them with tabs
              | "pre" `S.member` stack -> case maybeTabSize of
                                               Nothing -> tag : go stack rest
                                               Just tabSize -> TagText (collapseIntoTabs (tabSize - 1) text) : go stack rest
              | otherwise -> let text' = collapseSpaces text
                              in case text' of
                                      [] -> go stack rest
                                      _  -> TagText text' : go stack rest
            (tag : rest) -> tag : go stack rest

collapseSpaces :: String -> String
collapseSpaces = go
  where
    go [] = []
    go [c] = [c]
    go (c1 : c2 : rest)
      | isSpace c1 && isSpace c2 = go (c2 : rest)
      | otherwise = c1 : go (c2 : rest)

collapseIntoTabs :: Int -> String -> String
collapseIntoTabs n = go
  where
    go [] = []
    go (' ':cs)
      | (pref, rest) <- splitAt n cs
      , pref == pat = '\t' : go rest
    go (c:cs) = c : go cs

    pat = replicate n ' '
