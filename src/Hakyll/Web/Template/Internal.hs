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
    , readTemplateItem
    , unsafeReadTemplateFile

    , module Hakyll.Web.Template.Internal.Element
    , module Hakyll.Web.Template.Internal.Trim
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Except                 (MonadError (..))
import           Data.Binary                          (Binary)
import           Data.List                            (intercalate)
import           Data.Typeable                        (Typeable)
import           GHC.Exts                             (IsString (..))
import           GHC.Generics                         (Generic)
import           Prelude                              hiding (id)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
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
readTemplate :: String -> Template
readTemplate = readTemplateFile "{literal}"

--------------------------------------------------------------------------------
readTemplateItem :: Item String -> Template
readTemplateItem item = let file = show $ itemIdentifier item
                        in readTemplateFile file (itemBody item)

--------------------------------------------------------------------------------
readTemplateFile :: FilePath -> String -> Template
readTemplateFile origin =  template origin . readTemplateElemsFile origin

--------------------------------------------------------------------------------
-- | Read a template, without metadata header
templateBodyCompiler :: Compiler (Item Template)
templateBodyCompiler = cached "Hakyll.Web.Template.templateBodyCompiler" $ do
    item <- getResourceBody
    file <- getResourceFilePath
    return $ fmap (readTemplateFile file) item

--------------------------------------------------------------------------------
-- | Read complete file contents as a template
templateCompiler :: Compiler (Item Template)
templateCompiler = cached "Hakyll.Web.Template.templateCompiler" $ do
    item <- getResourceString
    file <- getResourceFilePath
    return $ fmap (readTemplateFile file) item


--------------------------------------------------------------------------------
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
applyTemplate' tes name context x = go tes `catchError` handler
  where
    context' :: String -> [String] -> Item a -> Compiler ContextField
    context' = unContext (context `mappend` missingField)

    itemName = show $ itemIdentifier x
    handler es = fail $ "Hakyll.Web.Template.applyTemplate: Failed to " ++
        (if name == itemName
          then "interpolate template in item " ++ name
          else "apply template " ++ name ++ " to item " ++ itemName) ++
        ":\n" ++ intercalate ",\n" es

    go = fmap concat . mapM applyElem

    trimError = fail $ "Hakyll.Web.Template.applyTemplate: template not " ++
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
applyAsTemplate context item = applyTemplate (readTemplateItem item) context item


--------------------------------------------------------------------------------
unsafeReadTemplateFile :: FilePath -> Compiler Template
unsafeReadTemplateFile file = do
    tpl <- unsafeCompiler $ readFile file
    pure $ readTemplateFile file tpl

