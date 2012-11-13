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
type Context a = String -> Identifier -> a -> Compiler String


--------------------------------------------------------------------------------
field :: String -> (Identifier -> a -> Compiler String) -> Context a
field key value k' id' x
    | k' == key = value id' x
    | otherwise = empty


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
bodyField key = field key $ \_ x -> return x


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
