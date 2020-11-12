--------------------------------------------------------------------------------
-- | Exports simple compilers to just copy files
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.File
    ( CopyFile (..)
    , copyFileCompiler
    , TmpFile (..)
    , newTmpFile
    , SymlinkFile (..)
    , symlinkFileCompiler
    ) where


--------------------------------------------------------------------------------
import           Data.Binary                   (Binary (..))
import           Data.Typeable                 (Typeable)
#if MIN_VERSION_directory(1,2,6)
import           System.Directory              (copyFileWithMetadata)
#else
import           System.Directory              (copyFile)
#endif
import           System.Directory              (doesFileExist,
                                                createFileLink,
                                                renameFile)
import           System.FilePath               ((</>))
import           System.Random                 (randomIO)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Configuration
import           Hakyll.Core.Item
import           Hakyll.Core.Provider
import qualified Hakyll.Core.Store             as Store
import           Hakyll.Core.Util.File
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | This will copy any file directly by using a system call
newtype CopyFile = CopyFile FilePath
    deriving (Binary, Eq, Ord, Show, Typeable)


--------------------------------------------------------------------------------
instance Writable CopyFile where
#if MIN_VERSION_directory(1,2,6)
    write dst (Item _ (CopyFile src)) = copyFileWithMetadata src dst
#else
    write dst (Item _ (CopyFile src)) = copyFile src dst
#endif
--------------------------------------------------------------------------------
copyFileCompiler :: Compiler (Item CopyFile)
copyFileCompiler = do
    identifier <- getUnderlying
    provider   <- compilerProvider <$> compilerAsk
    makeItem $ CopyFile $ resourceFilePath provider identifier

--------------------------------------------------------------------------------
-- | This will not copy a file but create a symlink, which can save space & time for static sites with many large static files which would normally be handled by copyFileCompiler. (Note: the user will need to make sure their sync method handles symbolic links correctly!)
newtype SymlinkFile = SymlinkFile FilePath
    deriving (Binary, Eq, Ord, Show, Typeable)
--------------------------------------------------------------------------------
instance Writable SymlinkFile where
    write dst (Item _ (SymlinkFile src)) = createFileLink src dst
--------------------------------------------------------------------------------
symlinkFileCompiler :: Compiler (Item SymlinkFile)
symlinkFileCompiler = do
    identifier <- getUnderlying
    provider   <- compilerProvider <$> compilerAsk
    makeItem $ SymlinkFile $ resourceFilePath provider identifier

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
