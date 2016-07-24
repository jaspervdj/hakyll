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
import           Data.List               (intercalate)
import           Data.Maybe              (isJust)
import           Data.Typeable           (Typeable)
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
instance Monoid Template where
    mempty = Template []
    (Template xs) `mappend` (Template ys) = Template (xs `mappend` ys)


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
    | TrimL
    | TrimR
    deriving (Show, Eq, Typeable)


--------------------------------------------------------------------------------
instance Binary TemplateElement where
    put (Chunk string) = putWord8 0 >> put string
    put (Expr e)       = putWord8 1 >> put e
    put  Escaped       = putWord8 2
    put (If e t f)     = putWord8 3 >> put e >> put t >> put f
    put (For e b s)    = putWord8 4 >> put e >> put b >> put s
    put (Partial e)    = putWord8 5 >> put e
    put  TrimL         = putWord8 6
    put  TrimR         = putWord8 7

    get = getWord8 >>= \tag -> case tag of
        0 -> Chunk <$> get
        1 -> Expr <$> get
        2 -> pure Escaped
        3 -> If <$> get <*> get <*> get
        4 -> For <$> get <*> get <*> get
        5 -> Partial <$> get
        6 -> pure TrimL
        7 -> pure TrimR
        _ -> error "Hakyll.Web.Template.Internal: Error reading cached template"


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
        _ -> error "Hakyll.Web.Template.Internal: Error reading cached template"


--------------------------------------------------------------------------------
(.~) :: [TemplateElement] -> Template -> Template
ts .~ (Template t) = Template (ts ++ t)

infixr 6 .~


--------------------------------------------------------------------------------
(~.) :: Template -> [TemplateElement] -> Template
(Template t) ~. ts = Template (t ++ ts)

infixl 5 ~.


--------------------------------------------------------------------------------
readTemplate :: String -> Template
readTemplate input = case P.parse template "" input of
    Left err -> error $ "Cannot parse template: " ++ show err
    Right t  -> t


--------------------------------------------------------------------------------
template :: P.Parser Template
template = mconcat <$> P.many (P.choice [ lift chunk
                                        , lift escaped
                                        , conditional
                                        , for
                                        , partial
                                        , expr
                                        ])
    where lift = fmap (Template . (:[]))


--------------------------------------------------------------------------------
chunk :: P.Parser TemplateElement
chunk = Chunk <$> P.many1 (P.noneOf "$")


--------------------------------------------------------------------------------
expr :: P.Parser Template
expr = P.try $ do
    trimLExpr <- trimOpen
    e <- expr'
    trimRExpr <- trimClose
    return $ [TrimL | trimLExpr] .~ Template [Expr e] ~. [TrimR | trimRExpr]


--------------------------------------------------------------------------------
expr' :: P.Parser TemplateExpr
expr' = stringLiteral <|> call <|> ident


--------------------------------------------------------------------------------
escaped :: P.Parser TemplateElement
escaped = Escaped <$ P.try (P.string "$$")


--------------------------------------------------------------------------------
trimOpen :: P.Parser Bool
trimOpen = do
    void $ P.char '$'
    trimLIf <- P.optionMaybe $ P.try (P.char '-')
    pure $ isJust trimLIf


--------------------------------------------------------------------------------
trimClose :: P.Parser Bool
trimClose = do
    trimIfR <- P.optionMaybe $ P.try (P.char '-')
    void $ P.char '$'
    pure $ isJust trimIfR


--------------------------------------------------------------------------------
conditional :: P.Parser Template
conditional = P.try $ do
    -- if
    trimLIf <- trimOpen
    void $ P.string "if("
    e <- expr'
    void $ P.char ')'
    trimRIf <- trimClose
    -- then
    thenBranch <- template
    -- else
    elseParse <- opt "else"
    -- endif
    trimLEnd <- trimOpen
    void $ P.string "endif"
    trimREnd <- trimClose

    -- As else is optional we need to sort out where any Trim_s need to go.
    let (thenBody, elseBody) = maybe (thenNoElse, Nothing) thenElse elseParse
            where thenNoElse =
                      [TrimR | trimRIf] .~ thenBranch ~. [TrimL | trimLEnd]

                  thenElse (trimLElse, elseBranch, trimRElse) = (thenB, elseB)
                      where thenB = [TrimR | trimRIf]
                                 .~ thenBranch
                                 ~. [TrimL | trimLElse]

                            elseB = Just $ [TrimR | trimRElse]
                                        .~ elseBranch
                                        ~. [TrimL | trimLEnd]

    pure $ [TrimL | trimLIf]
        .~ Template [If e thenBody elseBody]
        ~. [TrimR | trimREnd]


--------------------------------------------------------------------------------
for :: P.Parser Template
for = P.try $ do
    -- for
    trimLFor <- trimOpen
    void $ P.string "for("
    e <- expr'
    void $ P.char ')'
    trimRFor <- trimClose
    -- body
    bodyBranch <- template
    -- sep
    sepParse <- opt "sep"
    -- endfor
    trimLEnd <- trimOpen
    void $ P.string "endfor"
    trimREnd <- trimClose

    -- As sep is optional we need to sort out where any Trim_s need to go.
    let (forBody, sepBody) = maybe (forNoSep, Nothing) forSep sepParse
            where forNoSep =
                      [TrimR | trimRFor] .~ bodyBranch ~. [TrimL | trimLEnd]

                  forSep (trimLSep, sepBranch, trimRSep) = (forB, sepB)
                      where forB = [TrimR | trimRFor]
                                .~ bodyBranch
                                ~. [TrimL | trimLSep]

                            sepB = Just $ [TrimR | trimRSep]
                                       .~ sepBranch
                                       ~. [TrimL | trimLEnd]

    pure $ [TrimL | trimLFor]
        .~ Template [For e forBody sepBody]
        ~. [TrimR | trimREnd]


--------------------------------------------------------------------------------
partial :: P.Parser Template
partial = P.try $ do
    trimLPart <- trimOpen
    void $ P.string "partial("
    e <- expr'
    void $ P.char ')'
    trimRPart <- trimClose

    pure $ [TrimL | trimLPart] .~ Template [Partial e] ~. [TrimR | trimRPart]


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


--------------------------------------------------------------------------------
opt :: String -> P.Parser (Maybe (Bool, Template, Bool))
opt clause = P.optionMaybe $ P.try $ do
    trimL <- trimOpen
    void $ P.string clause
    trimR <- trimClose
    branch <- template
    pure (trimL, branch, trimR)

