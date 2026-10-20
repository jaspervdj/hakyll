--------------------------------------------------------------------------------
-- | Exports simple compilers to copy files or create links
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.File
    ( CopyFile (..)
    , copyFileCompiler
    , TmpFile (..)
    , newTmpFile
    , HardlinkFile (..)
    , hardlinkFileCompiler
    ) where


--------------------------------------------------------------------------------
import           Data.Binary                   (Binary (..))
import           Data.Typeable                 (Typeable)
import           System.Directory              (copyFileWithMetadata)
import           System.Directory              (doesFileExist,
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
import qualified Hakyll.Core.Util.File.Hardlink as Hardlink (createHardLink)
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | This will copy any file directly by using a system call
newtype CopyFile = CopyFile FilePath
    deriving (Binary, Eq, Ord, Show, Typeable)


--------------------------------------------------------------------------------
instance Writable CopyFile where
    write dst (Item _ (CopyFile src)) = copyFileWithMetadata src dst

--------------------------------------------------------------------------------
-- | Compile an item by copying its underlying file.
--
-- To create a hardlink instead, see `hardlinkFileCompiler`.
copyFileCompiler :: Compiler (Item CopyFile)
copyFileCompiler = do
    identifier <- getUnderlying
    provider   <- compilerProvider <$> compilerAsk
    makeItem $ CopyFile $ resourceFilePath provider identifier


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


--------------------------------------------------------------------------------
-- | This will not copy a file but create a hardlink, which can save space and
-- time for static sites with many large static files which would normally be
-- handled by `copyFileCompiler`.
--
-- Note that if you use hardlinks, you will need to make sure your sync method
-- handles hard links correctly. For example, if you use @rsync@, you will
-- need to provide the @--hard-links@ flag.
--
-- Moreover, if you use windows, creating hardlinks may not be supported by
-- your filesystem. Currently, only NTFS supports hardlinks on Windows.
newtype HardlinkFile = HardlinkFile FilePath
    deriving (Binary, Eq, Ord, Show, Typeable)


--------------------------------------------------------------------------------
instance Writable HardlinkFile where
    write dst (Item _ (HardlinkFile src))
        -- We use our own definition of `createHardLink` because
        -- it is not exposed for Windows in `directory`.
        = Hardlink.createHardLink src dst


--------------------------------------------------------------------------------
-- Compile an item by creating a hard link. This is useful when compiling
-- items which are large (e.g. PDFs or images).
--
-- Note that if you use hardlinks, you will need to make sure your sync method
-- handles hard links correctly. For example, if you use @rsync@, you will
-- need to provide the @--hard-links@ flag.
--
-- Moreover, if you use windows, creating hardlinks may not be supported by
-- your filesystem. Currently, only NTFS supports hardlinks on Windows.
--
-- To copy an item instead, see `copyFileCompiler`.
hardlinkFileCompiler :: Compiler (Item HardlinkFile)
hardlinkFileCompiler = do
    identifier <- getUnderlying
    provider   <- compilerProvider <$> compilerAsk
    makeItem $ HardlinkFile (resourceFilePath provider identifier)
