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
module Hakyll.Web.Template
    ( Template
    , applyTemplate
    , templateCompiler
    , templateCompilerWith
    , applyTemplateCompiler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                (forM, liftM)
import           Prelude                      hiding (id)
import           System.FilePath              (takeExtension)
import           Text.Hamlet                  (HamletSettings,
                                               defaultHamletSettings)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Web.Page.Internal
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.Internal
import           Hakyll.Web.Template.Read


--------------------------------------------------------------------------------
applyTemplate :: Monad m
              => (String -> a -> m String)
              -> Template -> a -> m String
applyTemplate context tpl x = liftM concat $
    forM (unTemplate tpl) $ \e -> case e of
        Chunk c -> return c
        Escaped -> return "$"
        Key k   -> context k x


--------------------------------------------------------------------------------
-- | Read a template. If the extension of the file we're compiling is
-- @.hml@ or @.hamlet@, it will be considered as a Hamlet template, and parsed
-- as such.
templateCompiler :: Compiler Template
templateCompiler = templateCompilerWith defaultHamletSettings


--------------------------------------------------------------------------------
-- | Version of 'templateCompiler' that enables custom settings.
templateCompilerWith :: HamletSettings -> Compiler Template
templateCompilerWith settings =
    cached "Hakyll.Web.Template.templateCompilerWith" $ do
        identifier <- getIdentifier
        string     <- getResourceString
        if takeExtension (toFilePath identifier) `elem` [".hml", ".hamlet"]
            -- Hamlet template
            then return $ readHamletTemplateWith settings string
            -- Hakyll template
            else return $ readTemplate string


--------------------------------------------------------------------------------
applyTemplateCompiler :: Template       -- ^ Template
                      -> Context Page   -- ^ Context
                      -> Page           -- ^ Page
                      -> Compiler Page  -- ^ Compiler
applyTemplateCompiler tpl context page = do
    identifier <- getIdentifier
    let context' k x = unContext context k identifier x
    applyTemplate context' tpl page
