--------------------------------------------------------------------------------
-- | Module containing the template data structure
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Template.Internal
    ( Template (..)
    , TemplateElement (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>))
import           Data.Binary          (Binary, get, getWord8, put, putWord8)
import           Data.List            (isPrefixOf)
import           Data.String          (IsString(..))
import           Data.Typeable        (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | Datatype used for template substitutions.
newtype Template = Template
    { unTemplate :: [TemplateElement]
    }
    deriving (Show, Eq, Binary, Typeable)


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
    deriving (Show, Eq, Typeable)

--------------------------------------------------------------------------------
instance Binary TemplateElement where
    put (Chunk string)    = putWord8 0 >> put string
    put (Key key)  = putWord8 1 >> put key
    put (Escaped) = putWord8 2

    get = getWord8 >>= \tag -> case tag of
            0 -> Chunk <$> get
            1 -> Key   <$> get
            2 -> return Escaped
            _ -> error $  "Hakyll.Web.Template.Internal: "
                       ++ "Error reading cached template"
                       
--------------------------------------------------------------------------------
instance IsString Template where
  fromString = Template . readTemplate'
    where readTemplate' [] = []
          readTemplate' string
            | "$$" `isPrefixOf` string =
              Escaped : readTemplate' (drop 2 string)
            | "$" `isPrefixOf` string =
                case readKey (drop 1 string) of
                  Just (key, rest) -> Key key : readTemplate' rest
                  Nothing          -> Chunk "$" : readTemplate' (drop 1 string)
            | otherwise =
                  let (chunk, rest) = break (== '$') string
                  in Chunk chunk : readTemplate' rest

          -- Parse an key into (key, rest) if it's valid, and return
          -- Nothing otherwise
          readKey string =
            let (key, rest) = span validKeyChar string
            in if not (null key) && "$" `isPrefixOf` rest
               then Just (key, drop 1 rest)
               else Nothing

          validKeyChar x = x `notElem` ['$', '\n', '\r']
