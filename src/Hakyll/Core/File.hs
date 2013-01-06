--------------------------------------------------------------------------------
-- | Exports simple compilers to just copy files
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.File
    ( CopyFile (..)
    , copyFileCompiler
    , TmpFile (..)
    , newTmpFile
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           ((<$>))
import           Data.Binary                   (Binary (..))
import           Data.Typeable                 (Typeable)
import           System.Directory              (copyFile, doesFileExist,
                                                renameFile)
import           System.FilePath               ((</>))
import           System.Random                 (randomIO)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Configuration
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import qualified Hakyll.Core.Store             as Store
import           Hakyll.Core.Util.File
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


--------------------------------------------------------------------------------
newtype TmpFile = TmpFile FilePath
    deriving (Typeable)


--------------------------------------------------------------------------------
instance Binary TmpFile where
    put _ = return ()
    get   = error $
        "Hakyll.Core.File.TmpFile: You tried to load a TmpFile, however, " ++
        "this is not possible since these are deleted as soon as possible."


--------------------------------------------------------------------------------
instance Writable TmpFile where
    write dst (Item _ (TmpFile fp)) = renameFile fp dst


--------------------------------------------------------------------------------
-- | Create a tmp file
newTmpFile :: String            -- ^ Suffix and extension
           -> Compiler TmpFile  -- ^ Resulting tmp path
newTmpFile suffix = do
    path <- mkPath
    compilerUnsafeIO $ makeDirectories path
    debugCompiler $ "newTmpFile " ++ path
    return $ TmpFile path
  where
    mkPath = do
        rand <- compilerUnsafeIO $ randomIO :: Compiler Int
        tmp  <- tmpDirectory . compilerConfig <$> compilerAsk
        let path = tmp </> Store.hash [show rand] ++ "-" ++ suffix
        exists <- compilerUnsafeIO $ doesFileExist path
        if exists then mkPath else return path
