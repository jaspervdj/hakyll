--------------------------------------------------------------------------------
module Hakyll.Web.Template.Context
    ( Context
    , field
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  (empty)
import           Control.Arrow
import           Hakyll.Core.Compiler


--------------------------------------------------------------------------------
type Context a = Compiler (String, a) String


--------------------------------------------------------------------------------
field :: String -> Compiler a String -> Context a
field key value = arr checkKey >>> empty ||| value
  where
    checkKey (k, x)
        | k == key  = Left ()
        | otherwise = Right x
