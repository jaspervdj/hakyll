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
--
-- Apart from interpolating @$key$@s from the 'Context' you can also
-- use the following macros:
--
-- * @$if(key)$@
--
-- > $if(key)$
-- >  <b> Defined </b>
-- > $else$
-- >  <b> Non-defined </b>
-- > $endif$
--
-- This example will print @Defined@ if @key@ is defined in the
-- context and @Non-defined@ otherwise. The @$else$@ clause is
-- optional.
--
-- * @$for(key)$@
--
-- The @for@ macro is used for enumerating 'Context' elements that are
-- lists, i.e. constructed using the 'listField' function. Assume that
-- in a context we have an element @listField \"key\" c itms@. Then
-- the snippet 
--
-- > $for(key)$
-- >   $x$
-- > $sep$,
-- > $endfor$
--
-- would, for each item @i@ in 'itms', lookup @$x$@ in the context @c@
-- with item @i@, interpolate it, and join the resulting list with
-- @,@.
--
-- Another concrete example one may consider is the following. Given the
-- context
--
-- > listField "things" (field "thing" (return . itemBody))
-- >    (sequence [makeItem "fruits", makeItem "vegetables"])
-- 
-- and a template
--
-- >  I like
-- >  $for(things)$
-- >    fresh $thing$$sep$, and 
-- >  $endfor$
--
-- the resulting page would look like
--
-- > <p>
-- >  I like
-- > 
-- >   fresh fruits, and 
-- > 
-- >   fresh vegetables
-- > </p>
--
-- The @$sep$@ part can be omitted. Usually, you can get by using the
-- 'applyListTemplate' and 'applyJoinListTemplate' functions.
--
-- * @$partial(path)$@
--
-- Loads a template located in a separate file and interpolates it
-- under the current context.
--
-- Assuming that the file @test.html@ contains
--
-- > <b>$key$</b>
--
-- The result of rendering
--
-- > <p>
-- >   $partial("test.html")$
-- > </p>
--
-- is the same as the result of rendering
--
-- > <p>
-- >   <b>$key$</b>
-- > </p>
--
-- That is, calling @$partial$@ is equivalent to just copying and pasting
-- template code.
--
{-# LANGUAGE ScopedTypeVariables #-}
module Hakyll.Web.Template
    ( Template
    , templateBodyCompiler
    , templateCompiler
    , applyTemplate
    , loadAndApplyTemplate
    , applyAsTemplate
    , readTemplate
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


--------------------------------------------------------------------------------
-- | Read a template, without metadata header
templateBodyCompiler :: Compiler (Item Template)
templateBodyCompiler = cached "Hakyll.Web.Template.templateBodyCompiler" $ do
    item <- getResourceBody
    return $ fmap readTemplate item

--------------------------------------------------------------------------------
-- | Read complete file contents as a template
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
applyTemplate'
    :: forall a.
       Template         -- ^ Template
    -> Context a        -- ^ Context
    -> Item a           -- ^ Page
    -> Compiler String  -- ^ Resulting item
applyTemplate' tpl context x = go tpl
  where
    context' :: String -> [String] -> Item a -> Compiler ContextField
    context' = unContext (context `mappend` missingField)

    go = liftM concat . mapM applyElem . unTemplate

    ---------------------------------------------------------------------------

    applyElem :: TemplateElement -> Compiler String

    applyElem (Chunk c) = return c

    applyElem (Expr e) = applyExpr e >>= getString e

    applyElem Escaped = return "$"

    applyElem (If e t mf) = (applyExpr e >> go t) `catchError` handler
      where
        handler _ = case mf of
            Nothing -> return ""
            Just f  -> go f

    applyElem (For e b s) = applyExpr e >>= \cf -> case cf of
        StringField _  -> fail $
            "Hakyll.Web.Template.applyTemplateWith: expected ListField but " ++
            "got StringField for expr " ++ show e
        ListField c xs -> do
            sep <- maybe (return "") go s
            bs  <- mapM (applyTemplate' b c) xs
            return $ intercalate sep bs

    applyElem (Partial e) = do
        p    <- applyExpr e >>= getString e
        tpl' <- loadBody (fromFilePath p)
        applyTemplate' tpl' context x

    ---------------------------------------------------------------------------

    applyExpr :: TemplateExpr -> Compiler ContextField

    applyExpr (Ident (TemplateKey k)) = context' k [] x

    applyExpr (Call (TemplateKey k) args) = do
        args' <- mapM (\e -> applyExpr e >>= getString e) args
        context' k args' x

    applyExpr (StringLiteral s) = return (StringField s)

    ----------------------------------------------------------------------------

    getString _ (StringField s) = return s
    getString e (ListField _ _) = fail $
        "Hakyll.Web.Template.applyTemplateWith: expected StringField but " ++
        "got ListField for expr " ++ show e


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
