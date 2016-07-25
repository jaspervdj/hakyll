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
-- In the examples above you can see that outputs contain a lot of leftover
-- whitespace that you may wish to remove. Using @'$-'@ or @'-$'@ instead of
-- @'$'@ in a macro strips all whitespace to the left or right of that clause
-- respectively. Given the context
--
-- > listField "counts" (field "count" (return . itemBody))
-- >    (sequence [makeItem "3", makeItem "2", makeItem "1"])
--
-- and a template
--
-- > <p>
-- >     $for(counts)-$
-- >       $count$
-- >       $-sep-$...
-- >     $-endfor$
-- > </p>
--
-- the resulting page would look like
--
-- > <p>
-- >     3...2...1
-- > </p>
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
import           Control.Monad.Except         (MonadError (..))
import           Data.Binary                  (Binary)
import           Data.List                    (intercalate)
import           Data.Typeable                (Typeable)
import           GHC.Exts                     (IsString (..))
import           Prelude                      hiding (id)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Writable
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.Internal
import           Hakyll.Web.Template.Trim


--------------------------------------------------------------------------------
-- | Datatype used for template substitutions.
newtype Template = Template
    { unTemplate :: [TemplateElement]
    } deriving (Show, Eq, Binary, Typeable)


--------------------------------------------------------------------------------
instance Writable Template where
    -- Writing a template is impossible
    write _ _ = return ()


--------------------------------------------------------------------------------
instance IsString Template where
    fromString = readTemplate


--------------------------------------------------------------------------------
readTemplate :: String -> Template
readTemplate = Template . trim . readTemplateElems


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
    body <- applyTemplate' (unTemplate tpl) context item
    return $ itemSetBody body item


--------------------------------------------------------------------------------
applyTemplate'
    :: forall a.
       [TemplateElement] -- ^ Unwrapped Template
    -> Context a         -- ^ Context
    -> Item a            -- ^ Page
    -> Compiler String   -- ^ Resulting item
applyTemplate' tes context x = go tes
  where
    context' :: String -> [String] -> Item a -> Compiler ContextField
    context' = unContext (context `mappend` missingField)

    go = fmap concat . mapM applyElem

    trimError = error $ "Hakyll.Web.Template.applyTemplate: template not " ++
        "fully trimmed."

    ---------------------------------------------------------------------------

    applyElem :: TemplateElement -> Compiler String

    applyElem TrimL = trimError

    applyElem TrimR = trimError

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
