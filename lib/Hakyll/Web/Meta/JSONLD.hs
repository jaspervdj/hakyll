{-# LANGUAGE OverloadedStrings #-}

{- |

JSON-LD metadata, using <https://schema.org/ Schema.org> vocabulary
for articles.  Google applications and other search engines use
these data to improve search results and links.

This implementation supports the following fields:

+-------------------+----------------------------------------------------+
| @\@type@          | __Hardcoded__ value @\"Article"@.                  |
+-------------------+----------------------------------------------------+
| @headline@        | __Required__ taken from context field @title@.     |
+-------------------+----------------------------------------------------+
| @datePublished@   | __Required__ date of publication, via 'dateField'. |
+-------------------+----------------------------------------------------+

To use, add a 'jsonldField' to your template context:

@
let
  context = 'defaultContext' <> …
  postContext =
    context
    <> 'jsonldField' "jsonld" context
@

And update the template:

@
\<head>
  \<title>$title$\</title>
  \<link rel="stylesheet" type="text\/css" href="\/css\/default.css" />
  $if(jsonld)$$jsonld("embed")$$endif$
\</head>
@

The @"embed"@ argument generates a @\<script …>@ tag to be directly
included in page HTML.  To get the raw JSON string, use @"raw"@
instead.

-}
module Hakyll.Web.Meta.JSONLD
  ( jsonldField
  ) where

import Data.Aeson ((.=), pairs)
import Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Item
import Hakyll.Web.Template
import Hakyll.Web.Template.Context

runContext :: Context String -> String -> Compiler String
runContext ctx k = do
  i <- makeItem "dummy"
  unContext ctx k [] i >>= \cf -> case cf of
    StringField s -> pure s
    _             -> fail $ "Error: '" <> k <> "' is not a StringField"

getContext :: Context String -> String -> Compiler String
getContext ctx k = compilerTry (runContext ctx k) >>= either f pure
  where
  f (CompilationNoResult _) = compilerResult . CompilerError . CompilationFailure . pure $
                              "missing required field '" <> k <> "'"
  f err = compilerResult (CompilerError err)

-- This may come in handy later
_lookupContext :: Context String -> String -> Compiler (Maybe String)
_lookupContext ctx k = compilerTry (runContext ctx k) >>= either f (pure . Just)
  where
  f (CompilationNoResult _) = pure Nothing
  f err = compilerResult (CompilerError err)

-- | Render JSON-LD for an article.
-- Requires context with "title", and the item must be able to yield
-- a valid date via 'getItemUTC'
--
renderJSONLD :: Context String -> Compiler (Item String)
renderJSONLD ctx = do
  dateString <- getContext (dateField "" "%Y-%m-%dT%H:%M:%S") ""
  titleString <- getContext ctx "title"

  let
    obj = pairs $
      "@context" .= ("https://schema.org" :: String)
      <> "@type" .= ("Article" :: String)
      <> "headline" .= titleString
      <> "datePublished" .= dateString

  makeItem . LT.unpack . LT.decodeUtf8 . encodingToLazyByteString $ obj

jsonldField :: String -> Context String -> Context String
jsonldField k ctx = functionField k (\args _i -> go args)
  where
  -- The zero argument case cannot be a compiler error,
  -- otherwise @$if(k)$@ evaluates false.
  go [] = pure $ "<!-- Whoops! Try this instead: $if(" <> k <> ")$$" <> k <> "(\"embed\")$$endif$ -->"
  go ["raw"] = itemBody <$> renderJSONLD ctx
  go ["embed"] = do
    template <- jsonldTemplate
    i <- renderJSONLD ctx >>= applyTemplate template (bodyField "body")
    pure $ itemBody i
  go [_] = fail $ "invalid argument to jsonldField '" <> k <> "'. use \"raw\" or \"embed\""
  go _ = fail $ "too many arguments to jsonldField '" <> k <> "'"

jsonldTemplate :: Compiler Template
jsonldTemplate = do
  makeItem "<script type=\"application/ld+json\">$body$</script>"
  >>= compileTemplateItem
