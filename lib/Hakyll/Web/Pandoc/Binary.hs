{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
module Hakyll.Web.Pandoc.Binary where

import           Data.Binary        (Binary (..))

import           Text.Pandoc

--------------------------------------------------------------------------------
-- orphans

instance Binary Alignment
instance Binary Block
instance Binary Caption
instance Binary Cell
instance Binary ColSpan
instance Binary ColWidth
instance Binary Citation
instance Binary CitationMode
instance Binary Format
instance Binary Inline
instance Binary ListNumberDelim
instance Binary ListNumberStyle
instance Binary MathType
instance Binary QuoteType
instance Binary Row
instance Binary RowHeadColumns
instance Binary RowSpan
instance Binary TableBody
instance Binary TableFoot
instance Binary TableHead
