--------------------------------------------------------------------------------
-- | Parser utilities
module Hakyll.Core.Util.Parser
    ( metadataKey
    , reservedKeys
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad       (mzero)
import qualified Text.Parsec         as P
import           Text.Parsec.String  (Parser)


--------------------------------------------------------------------------------
metadataKey :: Parser String
metadataKey = do
    i <- (:) <$> P.letter <*> (P.many $ P.alphaNum <|> P.oneOf " _-.")
    if i `elem` reservedKeys then mzero else return i


--------------------------------------------------------------------------------
reservedKeys :: [String]
reservedKeys = [ "if", "else", "endif", "for", "sep", "endfor", "partial"
               , "route"                                                
               ]
