--------------------------------------------------------------------------------
module Hakyll.Web.Template.Context
    ( Context
    , field

    , defaultContext
    , bodyField
    , urlField
    , pathField
    , categoryField
    , titleField
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    (empty, (<|>))
import           Control.Arrow
import           System.FilePath        (takeBaseName, takeDirectory)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Web.Urls


--------------------------------------------------------------------------------
type Context a = Compiler (String, a) String


--------------------------------------------------------------------------------
field :: String -> Compiler a String -> Context a
field key value = arr checkKey >>> empty ||| value
  where
    checkKey (k, x)
        | k == key  = Left ()
        | otherwise = Right x


--------------------------------------------------------------------------------
defaultContext :: Context (Identifier String, String)
defaultContext =
    bodyField     "body"     <|>
    urlField      "url"      <|>
    pathField     "path"     <|>
    categoryField "category" <|>
    titleField    "title"


--------------------------------------------------------------------------------
bodyField :: String -> Context (Identifier String, String)
bodyField key = field key $ arr snd


--------------------------------------------------------------------------------
urlField :: String -> Context (Identifier a, a)
urlField key = field key $ fst ^>> getRouteFor >>^ maybe empty toUrl


--------------------------------------------------------------------------------
pathField :: String -> Context (Identifier a, a)
pathField key = field key $ arr $ toFilePath . fst


--------------------------------------------------------------------------------
categoryField :: String -> Context (Identifier a, a)
categoryField key = pathField key >>^ (takeBaseName . takeDirectory)


--------------------------------------------------------------------------------
titleField :: String -> Context (Identifier a, a)
titleField key = pathField key >>^ takeBaseName
