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
{-# LANGUAGE Arrows              #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hakyll.Web.Template
    ( Template
    , applyTemplate
    , applyTemplateToPage
    , applySelf
    , templateCompiler
    , templateCompilerWith
    , applyTemplateCompiler
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Category             (id)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe)
import           Prelude                      hiding (id)
import           System.FilePath              (takeExtension)
import           Text.Hamlet                  (HamletSettings,
                                               defaultHamletSettings)

--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Util.Arrow
import           Hakyll.Web.Page.Internal
import           Hakyll.Web.Template.Internal
import           Hakyll.Web.Template.Read


--------------------------------------------------------------------------------
applyTemplate :: forall a b. (ArrowChoice a, ArrowMap a)
              => a (String, b) String
              -> a (Template, b) String
applyTemplate field =
    arr (\(tpl, x) -> [(e, x) | e <- unTemplate tpl]) >>>
    mapA applyElement >>^ concat
  where
    applyElement :: a (TemplateElement, b) String
    applyElement = unElement >>> (id ||| field)

    unElement :: a (TemplateElement, b) (Either String (String, b))
    unElement = arr $ \(e, x) -> case e of
        Chunk c -> Left c
        Escaped -> Left "$"
        Key k   -> Right (k, x)


--------------------------------------------------------------------------------
-- | TODO: Remove
applyTemplateToPage :: Template -> Page String -> Page String
applyTemplateToPage tpl page =
    fmap (const $ applyTemplate pageField (tpl, page)) page
  where
    pageField (k, p) = fromMaybe ("$" ++ k ++ "$") $ M.lookup k $ toMap p
{-# DEPRECATED applyTemplateToPage "Use applyTemplate" #-}


--------------------------------------------------------------------------------
-- | Apply a page as it's own template. This is often very useful to fill in
-- certain keys like @$root@ and @$url@.
applySelf :: Page String -> Page String
applySelf page = applyTemplateToPage (readTemplate $ pageBody page) page
{-# DEPRECATED applySelf "Use applyTemplate" #-}


--------------------------------------------------------------------------------
-- | Read a template. If the extension of the file we're compiling is
-- @.hml@ or @.hamlet@, it will be considered as a Hamlet template, and parsed
-- as such.
templateCompiler :: Compiler () Template
templateCompiler = templateCompilerWith defaultHamletSettings

-- | Version of 'templateCompiler' that enables custom settings.
--
templateCompilerWith :: HamletSettings -> Compiler () Template
templateCompilerWith settings =
    cached "Hakyll.Web.Template.templateCompilerWith" $
        getIdentifier &&& getResourceString >>^ uncurry read'
  where
    read' identifier string =
        if takeExtension (toFilePath identifier) `elem` [".hml", ".hamlet"]
            -- Hamlet template
            then readHamletTemplateWith settings string
            -- Hakyll template
            else readTemplate string


--------------------------------------------------------------------------------
applyTemplateCompiler :: Identifier Template                   -- ^ Template
                      -> Compiler (Page String) (Page String)  -- ^ Compiler
applyTemplateCompiler identifier = require identifier $
    flip applyTemplateToPage
