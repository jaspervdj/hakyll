--------------------------------------------------------------------------------
-- | Module containing the template data structure
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Template.Internal
    ( Template (..)
    , TemplateElement (..)
    , readTemplate
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative     (pure, (<$), (<$>), (<*>), (<|>))
import           Control.Monad           (void)
import           Data.Binary             (Binary, get, getWord8, put, putWord8)
import           Data.Typeable           (Typeable)
import           GHC.Exts                (IsString (..))
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.Parser
import           Hakyll.Core.Writable
import           Hakyll.Core.Identifier


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
-- | Elements of a template.
data TemplateElement
    = Chunk String
    | Key String
    | Escaped
    | If String Template (Maybe Template)   -- key, then branch, else branch
    | For String Template (Maybe Template)  -- key, body, separator
    | Partial String                        -- filename
    | RouteOf  Identifier                   -- the route of the identifier
    deriving (Show, Eq, Typeable)


--------------------------------------------------------------------------------
instance Binary TemplateElement where
    put (Chunk string) = putWord8 0 >> put string
    put (Key k)        = putWord8 1 >> put k
    put (Escaped)      = putWord8 2
    put (If k t f  )   = putWord8 3 >> put k >> put t >> put f
    put (For k b s)    = putWord8 4 >> put k >> put b >> put s
    put (Partial p)    = putWord8 5 >> put p
    put (RouteOf i)    = putWord8 6 >> put i
    get = getWord8 >>= \tag -> case tag of
        0 -> Chunk <$> get
        1 -> Key   <$> get
        2 -> pure Escaped
        3 -> If <$> get <*> get <*> get
        4 -> For <$> get <*> get <*> get
        5 -> Partial <$> get
        6 -> RouteOf <$> get
        _ -> error $
            "Hakyll.Web.Template.Internal: Error reading cached template"


--------------------------------------------------------------------------------
instance IsString Template where
    fromString = readTemplate


--------------------------------------------------------------------------------
readTemplate :: String -> Template
readTemplate input = case P.parse template "" input of
    Left err -> error $ "Cannot parse template: " ++ show err
    Right t  -> t


--------------------------------------------------------------------------------
template :: P.Parser Template
template = Template <$> P.many1 templateElement

templateElement :: P.Parser TemplateElement
templateElement =   chunk
                <|> escaped
                <|> conditional
                <|> for
                <|> partial
                <|> routeOf
                <|> key


--------------------------------------------------------------------------------
chunk :: P.Parser TemplateElement
chunk = Chunk <$> (P.many1 $ P.noneOf "$")


--------------------------------------------------------------------------------
escaped :: P.Parser TemplateElement
escaped = Escaped <$ (P.try $ P.string "$$")


--------------------------------------------------------------------------------
conditional :: P.Parser TemplateElement
conditional = P.try $ do
    void $ P.string "$if("
    i <- metadataKey
    void $ P.string ")$"
    thenBranch <- template
    elseBranch <- P.optionMaybe $ P.try (P.string "$else$") >> template
    void $ P.string "$endif$"
    return $ If i thenBranch elseBranch


--------------------------------------------------------------------------------
for :: P.Parser TemplateElement
for = P.try $ do
    void $ P.string "$for("
    i <- metadataKey
    void $ P.string ")$"
    body <- template
    sep  <- P.optionMaybe $ P.try (P.string "$sep$") >> template
    void $ P.string "$endfor$"
    return $ For i body sep


--------------------------------------------------------------------------------
partial :: P.Parser TemplateElement
partial = P.try $ do
    void $ P.string "$partial("
    i <- stringLiteral
    void $ P.string ")$"
    return $ Partial i


--------------------------------------------------------------------------------
key :: P.Parser TemplateElement
key = P.try $ do
    void $ P.char '$'
    k <- metadataKey
    void $ P.char '$'
    return $ Key k

--------------------------------------------------------------------------------

routeOf :: P.Parser TemplateElement
routeOf = P.try $ RouteOf <$> function "route" ident
  where ident = fromString <$> stringLiteral

--------------------------------------------------------------------------------
stringLiteral :: P.Parser String
stringLiteral = do
    void $ P.char '\"'
    str <- P.many $ do
        x <- P.noneOf "\""
        if x == '\\' then P.anyChar else return x
    void $ P.char '\"'
    return str


--------------------------------------------------------------------------------

-- | Parses stuff like $keyword(something)$.
function :: String -> P.Parser a -> P.Parser a
function fname prs = P.between open close prs
  where open  = P.string $ "$" ++ fname ++ "("
        close = P.string $ ")$"
