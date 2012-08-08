-- | Module containing the template data structure
--
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances  #-}
module Hakyll.Web.Template.Internal
    ( Template (..)
    , TemplateElement (..)
    , TemplateString (..)
    ) where

import Control.Applicative ((<$>))

import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Typeable (Typeable)
import Data.Monoid
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Hakyll.Core.Writable
import Hakyll.Core.Compiler
import Hakyll.Core.Resource
import Control.Arrow
import Data.String
import Data.List

-- | The typeclass of string-like types that can be used to build a template.
class (Monoid a, IsString a, Binary a, Show a, Eq a, Typeable a, Writable a, Ord a) => TemplateString a where
  tsNull :: a -> Bool
  tsIsPrefixOf :: a -> a -> Bool
  tsToString :: a -> String
  tsDrop :: Int -> a -> a
  tsBreak :: (Char -> Bool) -> a -> (a, a)
  tsBreak f = tsSpan (not . f)
  tsSpan :: (Char -> Bool) -> a -> (a, a)
  tsSpan f = tsBreak (not . f)
  tsGetResource :: Compiler Resource a

instance TemplateString [Char] where
  tsNull = null
  tsIsPrefixOf = isPrefixOf
  tsToString = id
  tsDrop = drop
  tsBreak = break
  tsSpan = span
  tsGetResource = getResourceString

instance TemplateString SBS.ByteString where
  tsNull = SBS.null
  tsIsPrefixOf = SBS.isPrefixOf
  tsToString = SBS.unpack
  tsDrop n = SBS.drop (fromIntegral n)
  tsBreak = SBS.break
  tsSpan = SBS.span
  tsGetResource = getResourceLBS >>^ (SBS.concat . LBS.toChunks)
  
instance TemplateString LBS.ByteString where
  tsNull = LBS.null
  tsIsPrefixOf = LBS.isPrefixOf
  tsToString = LBS.unpack
  tsDrop n = LBS.drop (fromIntegral n)
  tsBreak = LBS.break
  tsSpan = LBS.span
  tsGetResource = getResourceLBS

-- | Datatype used for template substitutions.
--
newtype Template a = Template
    { unTemplate :: [TemplateElement a]
    }
    deriving (Show, Eq, Binary, Typeable)

instance TemplateString a => Writable (Template a) where
    -- Writing a template is impossible
    write _ _ = return ()

-- | Elements of a template.
--
data TemplateElement a
    = Chunk a
    | Key String
    | Escaped
    deriving (Show, Eq, Typeable)

instance TemplateString a => Binary (TemplateElement a) where
    put (Chunk string)    = putWord8 0 >> put string
    put (Key key)  = putWord8 1 >> put key
    put (Escaped) = putWord8 2

    get = getWord8 >>= \tag -> case tag of
            0 -> Chunk <$> get
            1 -> Key   <$> get
            2 -> return Escaped
            _ -> error $  "Hakyll.Web.Template.Internal: "
                       ++ "Error reading cached template"
