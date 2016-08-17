{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}

module Hakyll.Web.Template.Internal
    ( Template
    , template
    , unTemplate
    , getOrigin
    , templateBodyCompiler
    , templateCompiler
    , applyTemplate
    , loadAndApplyTemplate
    , applyAsTemplate
    , readTemplate
    , compileTemplateItem
    , unsafeReadTemplateFile

    , module Hakyll.Web.Template.Internal.Element
    , module Hakyll.Web.Template.Internal.Trim
    ) where


--------------------------------------------------------------------------------
import           Data.Binary                          (Binary)
import           Data.List                            (intercalate)
import           Data.Typeable                        (Typeable)
import           GHC.Exts                             (IsString (..))
import           GHC.Generics                         (Generic)
import           Prelude                              hiding (id)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Logger                   (Verbosity (Error))
import           Hakyll.Core.Writable
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.Internal.Element
import           Hakyll.Web.Template.Internal.Trim


--------------------------------------------------------------------------------
-- | Datatype used for template substitutions.
data Template = Template
    { unTemplate :: [TemplateElement]
    , getOrigin  :: FilePath
    } deriving (Show, Eq, Generic, Binary, Typeable)


--------------------------------------------------------------------------------
instance Writable Template where
    -- Writing a template is impossible
    write _ _ = return ()


--------------------------------------------------------------------------------
instance IsString Template where
    fromString = readTemplate


--------------------------------------------------------------------------------
-- | Wrap the constructor to ensure trim is called.
template :: FilePath -> [TemplateElement] -> Template
template p = flip Template p . trim


--------------------------------------------------------------------------------
-- | Parse a string into a template.
-- You should prefer 'compileTemplateItem' over this.
readTemplate :: String -> Template
readTemplate = either error (template origin) . parseTemplateElemsFile origin
  where
    origin = "{literal}"

--------------------------------------------------------------------------------
-- | Parse an item body into a template.
-- Provides useful error messages in the 'Compiler' monad.
compileTemplateItem :: Item String -> Compiler Template
compileTemplateItem item = let file = itemIdentifier item
                           in compileTemplateFile file (itemBody item)

--------------------------------------------------------------------------------
compileTemplateFile :: Identifier -> String -> Compiler Template
compileTemplateFile file = either fail (return . template origin)
                         . parseTemplateElemsFile origin
  where
    origin = show file

--------------------------------------------------------------------------------
-- | Read a template, without metadata header
templateBodyCompiler :: Compiler (Item Template)
templateBodyCompiler = cached "Hakyll.Web.Template.templateBodyCompiler" $ do
    item <- getResourceBody
    file <- getUnderlying
    withItemBody (compileTemplateFile file) item

--------------------------------------------------------------------------------
-- | Read complete file contents as a template
templateCompiler :: Compiler (Item Template)
templateCompiler = cached "Hakyll.Web.Template.templateCompiler" $ do
    item <- getResourceString
    file <- getUnderlying
    withItemBody (compileTemplateFile file) item


--------------------------------------------------------------------------------
-- | Interpolate template expressions from context values in a page
applyTemplate :: Template                -- ^ Template
              -> Context a               -- ^ Context
              -> Item a                  -- ^ Page
              -> Compiler (Item String)  -- ^ Resulting item
applyTemplate tpl context item = do
    body <- applyTemplate' (unTemplate tpl) (getOrigin tpl) context item
    return $ itemSetBody body item


--------------------------------------------------------------------------------
applyTemplate'
    :: forall a.
       [TemplateElement] -- ^ Unwrapped Template
    -> FilePath          -- ^ template name
    -> Context a         -- ^ Context
    -> Item a            -- ^ Page
    -> Compiler String   -- ^ Resulting item
applyTemplate' tes name context x = go tes `compilerCatch` handler
  where
    context' :: String -> [String] -> Item a -> Compiler ContextField
    context' = unContext (context `mappend` missingField)

    itemName = show $ itemIdentifier x
    handler _ es = fail $ "Hakyll.Web.Template.applyTemplate: Failed to " ++
        (if name == itemName
          then "interpolate template in item " ++ name
          else "apply template " ++ name ++ " to item " ++ itemName) ++
        ":\n" ++ intercalate ",\n" es

    go = fmap concat . mapM applyElem

    ---------------------------------------------------------------------------

    applyElem :: TemplateElement -> Compiler String

    applyElem TrimL = trimError

    applyElem TrimR = trimError

    applyElem (Chunk c) = return c

    applyElem (Expr e) = applyExpr e >>= getString e

    applyElem Escaped = return "$"

    applyElem (If e t mf) = do
        c <- (applyExpr e >> return True) `compilerCatch` handler
        if c
            then go t
            else maybe (return "") go mf
      where
        handler Error es = compilerDebugLog (map (\err ->
            "Hakyll.Web.Template.applyTemplate: [ERROR] in 'if' condition " ++
            "for expr " ++ show e ++ ": " ++ err) es) >> return False
        handler _     _  = return False

    applyElem (For e b s) = applyExpr e >>= \cf -> case cf of
        NoField        -> expected "ListField" "boolField" e
        StringField _  -> expected "ListField" "StringField" e
        ListField c xs -> do
            sep <- maybe (return "") go s
            bs  <- mapM (applyTemplate' b name c) xs
            return $ intercalate sep bs

    applyElem (Partial e) = do
        p    <- applyExpr e >>= getString e
        tpl' <- loadBody (fromFilePath p)
        applyTemplate' (unTemplate tpl') (getOrigin tpl') context x

    ---------------------------------------------------------------------------

    applyExpr :: TemplateExpr -> Compiler ContextField

    applyExpr (Ident (TemplateKey k)) = context' k [] x

    applyExpr (Call (TemplateKey k) args) = do
        args' <- mapM (\e -> applyExpr e >>= getString e) args
        context' k args' x

    applyExpr (StringLiteral s) = return (StringField s)

    ----------------------------------------------------------------------------

    getString e NoField         = expected "StringField" "boolField" e
    getString _ (StringField s) = return s
    getString e (ListField _ _) = expected "StringField" "ListField" e

    expected typ act e = fail $ unwords ["Hakyll.Web.Template.applyTemplate:",
        "expected", typ, "but got", act, "for expr", show e]

    -- expected to never happen with all templates constructed by 'template'
    trimError = fail $
        "Hakyll.Web.Template.applyTemplate: template not fully trimmed."


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
applyAsTemplate context item = do
    tpl <- compileTemplateItem item
    applyTemplate tpl context item


--------------------------------------------------------------------------------
unsafeReadTemplateFile :: FilePath -> Compiler Template
unsafeReadTemplateFile file = do
    tpl <- unsafeCompiler $ readFile file
    compileTemplateFile (fromFilePath file) tpl
