--------------------------------------------------------------------------------
-- | Module containing the template data structure
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Template.Internal
    ( Template (..)
    , TemplateKey (..)
    , TemplateExpr (..)
    , TemplateElement (..)
    , readTemplate
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative     ((<|>))
import           Control.Monad           (void)
import           Data.Binary             (Binary, get, getWord8, put, putWord8)
import           Data.Typeable           (Typeable)
import           Data.List (intercalate)
import           GHC.Exts                (IsString (..))
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.Parser
import           Hakyll.Core.Writable


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
newtype TemplateKey = TemplateKey String
    deriving (Binary, Show, Eq, Typeable)


--------------------------------------------------------------------------------
instance IsString TemplateKey where
    fromString = TemplateKey


--------------------------------------------------------------------------------
-- | Elements of a template.
data TemplateElement
    = Chunk String
    | Expr TemplateExpr
    | Escaped
    | If TemplateExpr Template (Maybe Template)   -- expr, then, else
    | For TemplateExpr Template (Maybe Template)  -- expr, body, separator
    | Partial TemplateExpr                        -- filename
    deriving (Show, Eq, Typeable)


--------------------------------------------------------------------------------
instance Binary TemplateElement where
    put (Chunk string) = putWord8 0 >> put string
    put (Expr e)       = putWord8 1 >> put e
    put (Escaped)      = putWord8 2
    put (If e t f  )   = putWord8 3 >> put e >> put t >> put f
    put (For e b s)    = putWord8 4 >> put e >> put b >> put s
    put (Partial e)    = putWord8 5 >> put e

    get = getWord8 >>= \tag -> case tag of
        0 -> Chunk <$> get
        1 -> Expr <$> get
        2 -> pure Escaped
        3 -> If <$> get <*> get <*> get
        4 -> For <$> get <*> get <*> get
        5 -> Partial <$> get
        _ -> error $
            "Hakyll.Web.Template.Internal: Error reading cached template"


--------------------------------------------------------------------------------
-- | Expression in a template
data TemplateExpr
    = Ident TemplateKey
    | Call TemplateKey [TemplateExpr]
    | StringLiteral String
    deriving (Eq, Typeable)


--------------------------------------------------------------------------------
instance Show TemplateExpr where
    show (Ident (TemplateKey k))   = k
    show (Call (TemplateKey k) as) =
        k ++ "(" ++ intercalate ", " (map show as) ++ ")"
    show (StringLiteral s)         = show s


--------------------------------------------------------------------------------
instance Binary TemplateExpr where
    put (Ident k)         = putWord8 0 >> put k
    put (Call k as)       = putWord8 1 >> put k >> put as
    put (StringLiteral s) = putWord8 2 >> put s

    get = getWord8 >>= \tag -> case tag of
        0 -> Ident         <$> get
        1 -> Call          <$> get <*> get
        2 -> StringLiteral <$> get
        _ -> error $
            "Hakyll.Web.Tamplte.Internal: Error reading cached template"


--------------------------------------------------------------------------------
readTemplate :: String -> Template
readTemplate input = case P.parse topLevelTemplate "" input of
    Left err -> error $ "Cannot parse template: " ++ show err
    Right t  -> t

--------------------------------------------------------------------------------
topLevelTemplate :: P.Parser Template
topLevelTemplate = Template <$>
    P.manyTill templateElement P.eof

--------------------------------------------------------------------------------
template :: P.Parser Template
template = Template <$> P.many templateElement

--------------------------------------------------------------------------------
templateElement :: P.Parser TemplateElement
templateElement = chunk <|> escaped <|> conditional <|> for <|> partial <|> expr


--------------------------------------------------------------------------------
chunk :: P.Parser TemplateElement
chunk = Chunk <$> (P.many1 $ P.noneOf "$")


--------------------------------------------------------------------------------
expr :: P.Parser TemplateElement
expr = P.try $ do
    void $ P.char '$'
    e <- expr'
    void $ P.char '$'
    return $ Expr e


--------------------------------------------------------------------------------
expr' :: P.Parser TemplateExpr
expr' = stringLiteral <|> call <|> ident


--------------------------------------------------------------------------------
escaped :: P.Parser TemplateElement
escaped = Escaped <$ (P.try $ P.string "$$")


--------------------------------------------------------------------------------
conditional :: P.Parser TemplateElement
conditional = P.try $ do
    void $ P.string "$if("
    e <- expr'
    void $ P.string ")$"
    thenBranch <- template
    elseBranch <- P.optionMaybe $ P.try (P.string "$else$") >> template
    void $ P.string "$endif$"
    return $ If e thenBranch elseBranch


--------------------------------------------------------------------------------
for :: P.Parser TemplateElement
for = P.try $ do
    void $ P.string "$for("
    e <- expr'
    void $ P.string ")$"
    body <- template
    sep  <- P.optionMaybe $ P.try (P.string "$sep$") >> template
    void $ P.string "$endfor$"
    return $ For e body sep


--------------------------------------------------------------------------------
partial :: P.Parser TemplateElement
partial = P.try $ do
    void $ P.string "$partial("
    e <- expr'
    void $ P.string ")$"
    return $ Partial e


--------------------------------------------------------------------------------
ident :: P.Parser TemplateExpr
ident = P.try $ Ident <$> key


--------------------------------------------------------------------------------
call :: P.Parser TemplateExpr
call = P.try $ do
    f <- key
    void $ P.char '('
    P.spaces
    as <- P.sepBy expr' (P.spaces >> P.char ',' >> P.spaces)
    P.spaces
    void $ P.char ')'
    return $ Call f as


--------------------------------------------------------------------------------
stringLiteral :: P.Parser TemplateExpr
stringLiteral = do
    void $ P.char '\"'
    str <- P.many $ do
        x <- P.noneOf "\""
        if x == '\\' then P.anyChar else return x
    void $ P.char '\"'
    return $ StringLiteral str


--------------------------------------------------------------------------------
key :: P.Parser TemplateKey
key = TemplateKey <$> metadataKey
