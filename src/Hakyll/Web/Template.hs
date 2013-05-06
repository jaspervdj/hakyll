-- | This module provides means for reading and applying 'Template's.
--
-- Templates are tools to convert items into a string. They are perfectly suited
-- for laying out your site.
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
module Hakyll.Web.Template
    ( Template
    , templateCompiler
    , applyTemplate
    , loadAndApplyTemplate
    , applyAsTemplate
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                (liftM)
import           Control.Monad.Error          (MonadError (..))
import           Data.List                    (intercalate)
import           Data.Monoid                  (mappend)
import           Prelude                      hiding (id)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.Internal
import           Hakyll.Web.Template.Read


--------------------------------------------------------------------------------
-- | Read a template.
templateCompiler :: Compiler (Item Template)
templateCompiler = cached "Hakyll.Web.Template.templateCompiler" $ do
    item <- getResourceString
    return $ fmap readTemplate item


--------------------------------------------------------------------------------
applyTemplate :: Template                -- ^ Template
              -> Context a               -- ^ Context
              -> Item a                  -- ^ Page
              -> Compiler (Item String)  -- ^ Resulting item
applyTemplate tpl context item = do
    body <- applyTemplate' tpl context item
    return $ itemSetBody body item


--------------------------------------------------------------------------------
applyTemplate' :: Template         -- ^ Template
               -> Context a        -- ^ Context
               -> Item a           -- ^ Page
               -> Compiler String  -- ^ Resulting item
applyTemplate' tpl context x = go tpl
  where
    context' = unContext (context `mappend` missingField)
    go = liftM concat . mapM applyElem . unTemplate

    applyElem (Chunk c)   = return c
    applyElem Escaped     = return "$"
    applyElem (Key k)     = context' k x >>= getString k
    applyElem (If k t mf) = (context' k x >> go t) `catchError` handler
      where
        handler _ = case mf of
            Nothing -> return ""
            Just f  -> go f
    applyElem (For k b s) = context' k x >>= \cf -> case cf of
        StringField _  -> fail $
            "Hakyll.Web.Template.applyTemplateWith: expected ListField but " ++
            "got StringField for key " ++ show k
        ListField c xs -> do
            sep <- maybe (return "") go s
            bs  <- mapM (applyTemplate' b c) xs
            return $ intercalate sep bs

    getString _ (StringField s) = return s
    getString k (ListField _ _) = fail $
        "Hakyll.Web.Template.applyTemplateWith: expected StringField but " ++
        "got ListField for key " ++ show k


--------------------------------------------------------------------------------
-- | The following pattern is so common:
--
-- > tpl <- loadBody "templates/foo.html"
-- > someCompiler
-- >     >>= applyTemplate tpl context
--
-- That we have a single function which does this:
--
-- > someCompiler
-- >     >>= loadAndApplyTemplate "templates/foo.html" context
loadAndApplyTemplate :: Identifier              -- ^ Template identifier
                     -> Context a               -- ^ Context
                     -> Item a                  -- ^ Page
                     -> Compiler (Item String)  -- ^ Resulting item
loadAndApplyTemplate identifier context item = do
    tpl <- loadBody identifier
    applyTemplate tpl context item


--------------------------------------------------------------------------------
-- | It is also possible that you want to substitute @$key$@s within the body of
-- an item. This function does that by interpreting the item body as a template,
-- and then applying it to itself.
applyAsTemplate :: Context String          -- ^ Context
                -> Item String             -- ^ Item and template
                -> Compiler (Item String)  -- ^ Resulting item
applyAsTemplate context item =
    let tpl = readTemplate $ itemBody item
    in applyTemplate tpl context item
