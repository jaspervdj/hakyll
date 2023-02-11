--------------------------------------------------------------------------------
-- | Module containing the elements used in a template.  A template is generally
-- just a list of these elements.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Template.Internal.Element
    ( TemplateKey (..)
    , TemplateExpr (..)
    , TemplateElement (..)
    , templateElems
    , parseTemplateElemsFile
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative     ((<|>))
import           Control.Monad           (void)
import           Control.Arrow           (left)
import           Data.Binary             (Binary, get, getWord8, put, putWord8)
import           Data.List               (intercalate)
import           Data.Maybe              (isJust)
import           Data.Typeable           (Typeable)
import           GHC.Exts                (IsString (..))
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.Parser


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
      -- expr, then, else
    | If TemplateExpr [TemplateElement] (Maybe [TemplateElement])
      -- expr, body, separator
    | For TemplateExpr [TemplateElement] (Maybe [TemplateElement])
      -- filename
    | Partial TemplateExpr
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
parseTemplateElemsFile :: FilePath -> String -> Either String [TemplateElement]
parseTemplateElemsFile file = left (\e -> "Cannot parse template " ++ show e)
                            . P.parse (templateElems <* P.eof) file


--------------------------------------------------------------------------------
templateElems :: P.Parser [TemplateElement]
templateElems = mconcat <$> P.many (P.choice [ lift chunk
                                             , lift escaped
                                             , conditional
                                             , for
                                             , partial
                                             , expr
                                             ])
    where lift = fmap (:[])


--------------------------------------------------------------------------------
chunk :: P.Parser TemplateElement
chunk = Chunk <$> P.many1 (P.noneOf "$")


--------------------------------------------------------------------------------
expr :: P.Parser [TemplateElement]
expr = P.try $ do
    trimLExpr <- trimOpen
    e <- expr'
    trimRExpr <- trimClose
    return $ [TrimL | trimLExpr] ++ [Expr e] ++ [TrimR | trimRExpr]


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
    trimIfR <- P.optionMaybe $ (P.char '-')
    void $ P.char '$'
    pure $ isJust trimIfR


--------------------------------------------------------------------------------
conditional :: P.Parser [TemplateElement]
conditional = P.try $ do
    -- if
    trimLIf <- trimOpen
    void $ P.string "if("
    e <- expr'
    void $ P.char ')'
    trimRIf <- trimClose
    -- then
    thenBranch <- templateElems
    -- else
    elseParse <- opt "else"
    -- endif
    trimLEnd <- trimOpen
    void $ P.string "endif"
    trimREnd <- trimClose

    -- As else is optional we need to sort out where any Trim_s need to go.
    let (thenBody, elseBody) = maybe (thenNoElse, Nothing) thenElse elseParse
            where thenNoElse =
                      [TrimR | trimRIf] ++ thenBranch ++ [TrimL | trimLEnd]

                  thenElse (trimLElse, elseBranch, trimRElse) = (thenB, elseB)
                      where thenB = [TrimR | trimRIf]
                                 ++ thenBranch
                                 ++ [TrimL | trimLElse]

                            elseB = Just $ [TrimR | trimRElse]
                                        ++ elseBranch
                                        ++ [TrimL | trimLEnd]

    pure $ [TrimL | trimLIf] ++ [If e thenBody elseBody] ++ [TrimR | trimREnd]


--------------------------------------------------------------------------------
for :: P.Parser [TemplateElement]
for = P.try $ do
    -- for
    trimLFor <- trimOpen
    void $ P.string "for("
    e <- expr'
    void $ P.char ')'
    trimRFor <- trimClose
    -- body
    bodyBranch <- templateElems
    -- sep
    sepParse <- opt "sep"
    -- endfor
    trimLEnd <- trimOpen
    void $ P.string "endfor"
    trimREnd <- trimClose

    -- As sep is optional we need to sort out where any Trim_s need to go.
    let (forBody, sepBody) = maybe (forNoSep, Nothing) forSep sepParse
            where forNoSep =
                      [TrimR | trimRFor] ++ bodyBranch ++ [TrimL | trimLEnd]

                  forSep (trimLSep, sepBranch, trimRSep) = (forB, sepB)
                      where forB = [TrimR | trimRFor]
                                ++ bodyBranch
                                ++ [TrimL | trimLSep]

                            sepB = Just $ [TrimR | trimRSep]
                                       ++ sepBranch
                                       ++ [TrimL | trimLEnd]

    pure $ [TrimL | trimLFor] ++ [For e forBody sepBody] ++ [TrimR | trimREnd]


--------------------------------------------------------------------------------
partial :: P.Parser [TemplateElement]
partial = P.try $ do
    trimLPart <- trimOpen
    void $ P.string "partial("
    e <- expr'
    void $ P.char ')'
    trimRPart <- trimClose

    pure $ [TrimL | trimLPart] ++ [Partial e] ++ [TrimR | trimRPart]


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
opt :: String -> P.Parser (Maybe (Bool, [TemplateElement], Bool))
opt clause = P.optionMaybe $ P.try $ do
    trimL <- trimOpen
    void $ P.string clause
    trimR <- trimClose
    branch <- templateElems
    pure (trimL, branch, trimR)

