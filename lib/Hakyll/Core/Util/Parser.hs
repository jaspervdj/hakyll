--------------------------------------------------------------------------------
-- | Parser utilities
module Hakyll.Core.Util.Parser
    ( metadataKey
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Control.Monad       (guard, mzero, void)
import qualified Text.Parsec         as P
import           Text.Parsec.String  (Parser)


--------------------------------------------------------------------------------
metadataKey :: Parser String
metadataKey = do
    -- Ensure trailing '-' binds to '$' if present.
    let hyphon = P.try $ do
            void $ P.char '-'
            x <- P.lookAhead P.anyChar
            guard $ x /= '$'
            pure '-'

    i <- (:) <$> P.letter <*> P.many (P.alphaNum <|> P.oneOf "_." <|> hyphon)
    if i `elem` reservedKeys then mzero else return i


--------------------------------------------------------------------------------
reservedKeys :: [String]
reservedKeys = ["if", "else", "endif", "for", "sep", "endfor", "partial"]
