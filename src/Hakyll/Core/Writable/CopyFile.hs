--------------------------------------------------------------------------------
-- | Exports simple compilers to just copy files
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Writable.CopyFile
    ( CopyFile (..)
    , copyFileCompiler
    ) where


--------------------------------------------------------------------------------
import           Data.Binary            (Binary (..))
import           Data.Typeable          (Typeable)
import           System.Directory       (copyFile)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | This will copy any file directly by using a system call
data CopyFile = CopyFile
    deriving (Show, Eq, Ord, Typeable)


--------------------------------------------------------------------------------
instance Binary CopyFile where
    put CopyFile = return ()
    get          = return CopyFile


--------------------------------------------------------------------------------
instance Writable CopyFile where
    write dst item = copyFile (toFilePath $ itemIdentifier item) dst


--------------------------------------------------------------------------------
copyFileCompiler :: Compiler (Item CopyFile)
copyFileCompiler = makeItem CopyFile
