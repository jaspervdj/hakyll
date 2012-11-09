--------------------------------------------------------------------------------
-- | Exports simple compilers to just copy files
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Hakyll.Core.Writable.CopyFile
    ( CopyFile (..)
    , copyFileCompiler
    ) where


--------------------------------------------------------------------------------
import Control.Arrow ((>>^))
import System.Directory (copyFile)
import Data.Typeable (Typeable)
import Data.Binary (Binary)


--------------------------------------------------------------------------------
import Hakyll.Core.Writable
import Hakyll.Core.Compiler
import Hakyll.Core.Identifier


--------------------------------------------------------------------------------
-- | Newtype construct around 'FilePath' which will copy the file directly
newtype CopyFile = CopyFile {unCopyFile :: FilePath}
    deriving (Show, Eq, Ord, Binary, Typeable)


--------------------------------------------------------------------------------
instance Writable CopyFile where
    write dst (CopyFile src) = copyFile src dst


--------------------------------------------------------------------------------
copyFileCompiler :: Compiler a CopyFile
copyFileCompiler = getIdentifier >>^ CopyFile . toFilePath
