-- | Wraps pandocs bibiliography handling
{-# LANGUAGE Arrows, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Pandoc.Biblio
    ( CSL
    , cslCompiler
    , References (..)
    , referencesCompiler
    , processBiblioCompiler
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (arr, returnA)
import Data.Typeable (Typeable)

import Data.Binary (Binary (..))
import Text.Pandoc (Pandoc)
import Text.Pandoc.Biblio (processBiblio)
import qualified Text.CSL as CSL

import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Resource
import Hakyll.Core.Writable
import Hakyll.Web.Page

newtype CSL = CSL FilePath
    deriving (Binary, Show, Typeable, Writable)

cslCompiler :: Compiler Resource CSL
cslCompiler = arr (CSL . unResource)

newtype References = References [CSL.Reference]
    deriving (Show, Typeable)

instance Binary References where
    -- Ugly.
    get                 = References . read <$> get
    put (References rs) = put $ show rs

instance Writable References where
    write _ _ = return ()

referencesCompiler :: Compiler Resource References
referencesCompiler = unsafeCompiler $
    fmap References . CSL.readBiblioFile . unResource

processBiblioCompiler :: Identifier CSL
                      -> Identifier References
                      -> Compiler (Page Pandoc) (Page Pandoc)
processBiblioCompiler csl refs = proc page -> do
    let body = pageBody page
    CSL csl' <- require_ csl -< ()
    References refs' <- require_ refs -< ()
    body' <- unsafeCompiler (tuc processBiblio) -< (csl', refs', body)
    returnA -< page {pageBody = body'}
  where
    tuc f (x, y, z) = f x y z
