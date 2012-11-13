--------------------------------------------------------------------------------
module Hakyll.Web.Template.Context
    ( Context (..)
    , mapContext
    , field

    , defaultContext
    , bodyField
    , urlField
    , pathField
    , categoryField
    , titleField
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative      (Alternative (..), (<$>))
import           Data.Monoid              (Monoid (..))
import           System.FilePath          (takeBaseName, takeDirectory)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Web.Page.Internal
import           Hakyll.Web.Urls


--------------------------------------------------------------------------------
newtype Context a = Context
    { unContext :: String -> Identifier -> a -> Compiler String
    }


--------------------------------------------------------------------------------
instance Monoid (Context a) where
    mempty                          = Context $ \_ _ _ -> empty
    mappend (Context f) (Context g) = Context $ \k i x -> f k i x <|> g k i x


--------------------------------------------------------------------------------
mapContext :: (String -> String) -> Context a -> Context a
mapContext f (Context g) = Context $ \k i x -> f <$> g k i x


--------------------------------------------------------------------------------
field :: String -> (Identifier -> a -> Compiler String) -> Context a
field key value = Context $ \k i x -> if k == key then value i x else empty


--------------------------------------------------------------------------------
defaultContext :: Context Page
defaultContext =
    bodyField     "body"     `mappend`
    urlField      "url"      `mappend`
    pathField     "path"     `mappend`
    categoryField "category" `mappend`
    titleField    "title"    `mappend`
    missingField


--------------------------------------------------------------------------------
bodyField :: String -> Context Page
bodyField key = field key $ \_ x -> return x


--------------------------------------------------------------------------------
urlField :: String -> Context a
urlField key = field key $ \i _ -> maybe empty toUrl <$> getRouteFor i


--------------------------------------------------------------------------------
pathField :: String -> Context a
pathField key = field key $ \i _ -> return $ toFilePath i


--------------------------------------------------------------------------------
categoryField :: String -> Context a
categoryField key = mapContext (takeBaseName . takeDirectory) $ pathField key


--------------------------------------------------------------------------------
titleField :: String -> Context a
titleField key = mapContext takeBaseName $ pathField key


--------------------------------------------------------------------------------
missingField :: Context a
missingField = Context $ \k _ _ -> return $ "$" ++ k ++ "$"
