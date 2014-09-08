{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric              #-}
module Hakyll.Web.Pandoc.Binary where

import           Data.Binary            (Binary (..))

import qualified Text.CSL               as CSL
import qualified Text.CSL.Reference     as REF
import qualified Text.CSL.Style         as STY
import           Text.Pandoc            

--------------------------------------------------------------------------------
-- orphans

instance Binary REF.CNum
instance Binary REF.Literal
instance Binary REF.RefDate
instance Binary REF.RefType
instance Binary STY.Agent
instance Binary STY.Formatted
instance Binary Inline
instance Binary Block
instance Binary Citation
instance Binary MathType
instance Binary Alignment
instance Binary CitationMode
instance Binary QuoteType
instance Binary Format
instance Binary ListNumberDelim
instance Binary ListNumberStyle
instance Binary CSL.Reference
