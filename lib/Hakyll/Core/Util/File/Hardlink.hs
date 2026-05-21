{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------
-- | Exports a function to create hard links.
--
-- This module is separated because it involves
-- OS-specific code.
module Hakyll.Core.Util.File.Hardlink (createHardLink) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.Win32.HardLink as Windows (createHardLink)
#else
import qualified System.Posix.Files as Posix (createLink)
#endif

{- | @`createHardLink` src dst@ creates a hard link between @src@ and @dst@.

This function may throw OS-specific exceptions; see
`System.Win32.HardLink.createHardLink` for Windows, and
`System.Posix.Files.createLink` for POSIX-compliant OSes.
-}
createHardLink :: FilePath -> FilePath -> IO ()
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
createHardLink = Windows.createHardLink
#else
createHardLink = Posix.createLink
#endif