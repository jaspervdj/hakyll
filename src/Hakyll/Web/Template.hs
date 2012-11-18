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
    , templateCompiler
    , templateCompilerWith
    , applyTemplate
    , applyTemplateWith
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
import           Hakyll.Core.Item
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.Internal
import           Hakyll.Web.Template.Read


--------------------------------------------------------------------------------
-- | Read a template. If the extension of the file we're compiling is
-- @.hml@ or @.hamlet@, it will be considered as a Hamlet template, and parsed
-- as such.
templateCompiler :: Compiler (Item Template)
templateCompiler = templateCompilerWith defaultHamletSettings


--------------------------------------------------------------------------------
-- | Version of 'templateCompiler' that enables custom settings.
templateCompilerWith :: HamletSettings -> Compiler (Item Template)
templateCompilerWith settings =
    cached "Hakyll.Web.Template.templateCompilerWith" $ do
        identifier <- getUnderlying
        item       <- getResourceString
        if takeExtension (toFilePath identifier) `elem` [".hml", ".hamlet"]
            -- Hamlet template
            then return $ fmap (readHamletTemplateWith settings) item
            -- Hakyll template
            else return $ fmap readTemplate item


--------------------------------------------------------------------------------
applyTemplate :: Template                -- ^ Template
              -> Context a               -- ^ Context
              -> Item a                  -- ^ Page
              -> Compiler (Item String)  -- ^ Resulting item
applyTemplate tpl context item = do
    let context' k x = unContext context k x
    body <- applyTemplateWith context' tpl item
    return $ itemSetBody body item


--------------------------------------------------------------------------------
applyTemplateWith :: Monad m
                  => (String -> a -> m String)
                  -> Template -> a -> m String
applyTemplateWith context tpl x = liftM concat $
    forM (unTemplate tpl) $ \e -> case e of
        Chunk c -> return c
        Escaped -> return "$"
        Key k   -> context k x
