--------------------------------------------------------------------------------
-- | Exports simple compilers to just copy files
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Writable.CopyFile
    ( CopyFile (..)
    , copyFileCompiler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Data.Binary            (Binary)
import           Data.Typeable          (Typeable)
import           System.Directory       (copyFile)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | Newtype construct around 'FilePath' which will copy the file directly
newtype CopyFile = CopyFile {unCopyFile :: FilePath}
    deriving (Show, Eq, Ord, Binary, Typeable)


--------------------------------------------------------------------------------
instance Writable CopyFile where
    write dst (CopyFile src) = copyFile src dst


--------------------------------------------------------------------------------
copyFileCompiler :: Compiler CopyFile
copyFileCompiler = CopyFile . toFilePath <$> getIdentifier
