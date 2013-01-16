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
