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
import           Control.Applicative      (empty, (<|>))
import           Control.Arrow
import           System.FilePath          (takeBaseName, takeDirectory)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Web.Page.Internal
import           Hakyll.Web.Urls


--------------------------------------------------------------------------------
type Context a = Compiler (String, (Identifier a, a)) String


--------------------------------------------------------------------------------
field :: String -> Compiler (Identifier a, a) String -> Context a
field key value = arr checkKey >>> (empty ||| value)
  where
    checkKey (k, x)
        | k /= key  = Left ()
        | otherwise = Right x


--------------------------------------------------------------------------------
defaultContext :: Context Page
defaultContext =
    bodyField     "body"     <|>
    urlField      "url"      <|>
    pathField     "path"     <|>
    categoryField "category" <|>
    titleField    "title"    <|>
    missingField


--------------------------------------------------------------------------------
bodyField :: String -> Context Page
bodyField key = field key $ arr snd


--------------------------------------------------------------------------------
urlField :: String -> Context a
urlField key = field key $ fst ^>> getRouteFor >>^ maybe empty toUrl


--------------------------------------------------------------------------------
pathField :: String -> Context a
pathField key = field key $ arr $ toFilePath . fst


--------------------------------------------------------------------------------
categoryField :: String -> Context a
categoryField key = pathField key >>^ (takeBaseName . takeDirectory)


--------------------------------------------------------------------------------
titleField :: String -> Context a
titleField key = pathField key >>^ takeBaseName


--------------------------------------------------------------------------------
missingField :: Context a
missingField = arr $ \(k, _) -> "$" ++ k ++ "$"
