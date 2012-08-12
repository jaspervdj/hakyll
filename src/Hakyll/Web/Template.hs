-- | This module provides means for reading and applying 'Template's.
--
-- Templates are tools to convert data (pages) into a string. They are
-- perfectly suited for laying out your site.
--
-- Let's look at an example template:
--
-- > <html>
-- >     <head>
-- >         <title>My crazy homepage - $title$</title>
-- >     </head>
-- >     <body>
-- >         <div id="header">
-- >             <h1>My crazy homepage - $title$</h1>
-- >         </div>
-- >         <div id="content">
-- >             $body$
-- >         </div>
-- >         <div id="footer">
-- >             By reading this you agree that I now own your soul
-- >         </div>
-- >     </body>
-- > </html>
--
-- We can use this template to render a 'Page' which has a body and a @$title$@
-- metadata field.
--
-- As you can see, the format is very simple -- @$key$@ is used to render the
-- @$key$@ field from the page, everything else is literally copied. If you want
-- to literally insert @\"$key$\"@ into your page (for example, when you're
-- writing a Hakyll tutorial) you can use
--
-- > <p>
-- >     A literal $$key$$.
-- > </p>
--
-- Because of it's simplicity, these templates can be used for more than HTML:
-- you could make, for example, CSS or JS templates as well.
--
-- In addition to the native format, Hakyll also supports hamlet templates. For
-- more information on hamlet templates, please refer to:
-- <http://hackage.haskell.org/package/hamlet>. Internally, hamlet templates are
-- converted to hakyll templates -- which means that you can only use variable
-- insertion (and not all hamlet's features).
--
-- This is an example of a valid hamlet template. You should place them in
-- files with a @.hamlet@ extension:
--
-- > !!!
-- > <html>
-- >     <head>
-- >         <meta charset="UTF-8">
-- >         <title> MyAweSomeCompany - #{title}
-- >     <body>
-- >         <h1> MyAweSomeCompany - #{title}
-- >         <div id="navigation">
-- >             <a href="/index.html"> Home
-- >             <a href="/about.html"> About
-- >             <a href="/code.html"> Code
-- >         #{body}
--
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Template
    ( Template
    , TemplateString
    , tsToString
    , applyTemplate
    , applyTemplateWith
    , applySelf
    , templateCompiler
    , templateCompilerWith
    , applyTemplateCompiler
    , applyTemplateCompilerWith
    ) where

import Control.Arrow
import Control.Applicative
import Data.Maybe (fromMaybe)
import System.FilePath (takeExtension)
import qualified Data.Map as M
import Data.String
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as LBS

import Text.Hamlet (HamletSettings, defaultHamletSettings)

import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Resource
import Hakyll.Web.Template.Internal
import Hakyll.Web.Template.Read
import Hakyll.Web.Page.Internal

-- | Substitutes @$identifiers@ in the given @Template@ by values from the given
-- "Page". When a key is not found, it is left as it is.
--
applyTemplate :: TemplateString a => Template a -> Page a -> Page a
applyTemplate = applyTemplateWith defaultMissingHandler

-- | Default solution if a key is missing: render it again
defaultMissingHandler :: TemplateString a => String -> a
defaultMissingHandler k = "$" <> fromString k <> "$"

-- | A version of 'applyTemplate' which allows you to give a fallback option,
-- which can produce the value for a key if it is missing.
--
applyTemplateWith :: TemplateString a =>
  (String -> a)  -- ^ Fallback if key missing
  -> Template a          -- ^ Template to apply
  -> Page a              -- ^ Input page
  -> Page a              -- ^ Resulting page
applyTemplateWith missing template page =
    page { pageBody = mconcat $ map substitute (unTemplate template) }
  where
    substitute (Chunk chunk) = chunk
    substitute (Key key)
      | key == "body" = pageBody page
      | otherwise = fromMaybe (missing key) (fromString <$> M.lookup key (pageMetadata page))
    substitute (Escaped)     = "$"

-- | Apply a page as it's own template. This is often very useful to fill in
-- certain keys like @$root@ and @$url@.
--
applySelf :: TemplateString a => Page a -> Page a
applySelf page = applyTemplate (readTemplate $ pageBody page) page

-- | Read a template. If the extension of the file we're compiling is
-- @.hml@ or @.hamlet@, it will be considered as a Hamlet template, and parsed
-- as such.
--
templateCompiler :: TemplateString a => Compiler Resource (Template a)
templateCompiler = templateCompilerWith defaultHamletSettings

-- | Version of 'templateCompiler' that enables custom settings.
--
templateCompilerWith :: TemplateString a => HamletSettings -> Compiler Resource (Template a)
templateCompilerWith settings =
    cached "Hakyll.Web.Template.templateCompilerWith" $
        getIdentifier &&& tsGetResource >>^ uncurry read'
  where
    read' identifier string =
        if takeExtension (toFilePath identifier) `elem` [".hml", ".hamlet"]
            -- Hamlet template
            then readHamletTemplateWith settings (tsToString string)
            -- Hakyll template
            else readTemplate string

applyTemplateCompiler :: TemplateString a =>
  Identifier (Template a)       -- ^ Template
  -> Compiler (Page a) (Page a) -- ^ Compiler
applyTemplateCompiler = applyTemplateCompilerWith defaultMissingHandler

-- | A version of 'applyTemplateCompiler' which allows you to pass a function
-- which is called for a key when it is missing.
--
applyTemplateCompilerWith :: TemplateString a =>
  (String -> a)
  -> Identifier (Template a)
  -> Compiler (Page a) (Page a)
applyTemplateCompilerWith missing identifier =
    require identifier (flip $ applyTemplateWith missing)
