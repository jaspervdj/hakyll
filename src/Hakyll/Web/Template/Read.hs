--------------------------------------------------------------------------------
-- | Read templates in Hakyll's native format
module Hakyll.Web.Template.Read
    ( readTemplate
    ) where

--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<$), (<*>))
import           Control.Monad                (void, mzero)
import           Text.Parsec
import           Text.Parsec.String

--------------------------------------------------------------------------------
import           Hakyll.Web.Template.Internal

--------------------------------------------------------------------------------

readTemplate :: String -> Template
readTemplate input =
  case parse template "" input of
    Left err -> error $ "Cannot parse template: " ++ show err
    Right t -> t

template :: Parser Template
template = Template <$>
  (many1 $ chunk <|> escaped <|> conditional <|> key)

chunk :: Parser TemplateElement
chunk = Chunk <$> (many1 $ noneOf "$")

escaped :: Parser TemplateElement
escaped = Escaped <$ (try $ string "$$")

conditional :: Parser TemplateElement
conditional = try $ do
  void $ string "$if("
  i <- ident
  void $ string ")$"
  thenBranch <- template
  elseBranch <- optionMaybe $ try (string "$else$") >> template
  void $ string "$endif$"
  return $ If i thenBranch elseBranch

ident :: Parser String
ident = do
  i <- (:) <$> letter <*> (many $ alphaNum <|> oneOf " _-.")
  if i `elem` reserved
     then mzero
     else return i

reserved :: [String]
reserved = ["if", "else","endif"]

key :: Parser TemplateElement
key = try $ do
  void $ char '$'
  k <- ident
  void $ char '$'
  return $ Key k
