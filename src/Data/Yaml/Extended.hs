module Data.Yaml.Extended
    ( module Data.Yaml
    , toString
    , toList
    ) where

import qualified Data.Text   as T
import qualified Data.Vector as V
import           Data.Yaml

toString :: Value -> Maybe String
toString (String t)     = Just (T.unpack t)
toString (Bool   True)  = Just "true"
toString (Bool   False) = Just "false"
toString (Number d)     = Just (show d)
toString _              = Nothing

toList :: Value -> Maybe [Value]
toList (Array a) = Just (V.toList a)
toList _         = Nothing
