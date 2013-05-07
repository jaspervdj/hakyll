--------------------------------------------------------------------------------
-- | Read templates in Hakyll's native format
module Hakyll.Web.Template.Read
    ( readTemplate
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative          ((<$), (<$>))
import           Control.Monad                (void)
import           Text.Parsec
import           Text.Parsec.String


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.Parser
import           Hakyll.Web.Template.Internal


--------------------------------------------------------------------------------
readTemplate :: String -> Template
readTemplate input = case parse template "" input of
    Left err -> error $ "Cannot parse template: " ++ show err
    Right t -> t


--------------------------------------------------------------------------------
template :: Parser Template
template = Template <$>
    (many1 $ chunk <|> escaped <|> conditional <|> for <|> partial <|> key)


--------------------------------------------------------------------------------
chunk :: Parser TemplateElement
chunk = Chunk <$> (many1 $ noneOf "$")


--------------------------------------------------------------------------------
escaped :: Parser TemplateElement
escaped = Escaped <$ (try $ string "$$")


--------------------------------------------------------------------------------
conditional :: Parser TemplateElement
conditional = try $ do
    void $ string "$if("
    i <- metadataKey
    void $ string ")$"
    thenBranch <- template
    elseBranch <- optionMaybe $ try (string "$else$") >> template
    void $ string "$endif$"
    return $ If i thenBranch elseBranch


--------------------------------------------------------------------------------
for :: Parser TemplateElement
for = try $ do
    void $ string "$for("
    i <- metadataKey
    void $ string ")$"
    body <- template
    sep  <- optionMaybe $ try (string "$sep$") >> template
    void $ string "$endfor$"
    return $ For i body sep


--------------------------------------------------------------------------------
partial :: Parser TemplateElement
partial = try $ do
    void $ string "$partial("
    i <- stringLiteral
    void $ string ")$"
    return $ Partial i


--------------------------------------------------------------------------------
key :: Parser TemplateElement
key = try $ do
    void $ char '$'
    k <- metadataKey
    void $ char '$'
    return $ Key k


--------------------------------------------------------------------------------
stringLiteral :: Parser String
stringLiteral = do
    void $ char '\"'
    str <- many $ do
        x <- noneOf "\""
        if x == '\\' then anyChar else return x
    void $ char '\"'
    return str
