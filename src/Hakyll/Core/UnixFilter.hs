--------------------------------------------------------------------------------
-- | A Compiler that supports unix filters.
module Hakyll.Core.UnixFilter
    ( unixFilter
    , unixFilterLBS
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent   (forkIO)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import           System.IO            (Handle, hClose, hGetContents, hPutStr,
                                       hSetEncoding, localeEncoding)
import           System.Posix.IO      (closeFd, createPipe, dupTo, fdToHandle,
                                       stdInput, stdOutput)
import           System.Posix.Process (executeFile, forkProcess)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler


--------------------------------------------------------------------------------
-- | Use a unix filter as compiler. For example, we could use the 'rev' program
-- as a compiler.
--
-- > rev :: Compiler String
-- > rev = getResourceString >>= withItemBody (unixFilter "rev" [])
--
-- A more realistic example: one can use this to call, for example, the sass
-- compiler on CSS files. More information about sass can be found here:
--
-- <http://sass-lang.com/>
--
-- The code is fairly straightforward, given that we use @.scss@ for sass:
--
-- > match "style.scss" $ do
-- >     route   $ setExtension "css"
-- >     compile $ getResourceString >>=
-- >         withItemBody (unixFilter "sass" ["-s", "--scss"]) >>=
-- >         return . fmap compressCss
unixFilter :: String           -- ^ Program name
           -> [String]         -- ^ Program args
           -> String           -- ^ Program input
           -> Compiler String  -- ^ Program output
unixFilter = unixFilterWith writer reader
  where
    writer handle input = do
        hSetEncoding handle localeEncoding
        hPutStr handle input
    reader handle = do
        hSetEncoding handle localeEncoding
        hGetContents handle


--------------------------------------------------------------------------------
-- | Variant of 'unixFilter' that should be used for binary files
--
-- > match "music.wav" $ do
-- >     route   $ setExtension "ogg"
-- >     compile $ getResourceLBS >>= withItemBody (unixFilter "oggenc" ["-"])
unixFilterLBS :: String               -- ^ Program name
              -> [String]             -- ^ Program args
              -> ByteString           -- ^ Program input
              -> Compiler ByteString  -- ^ Program output
unixFilterLBS = unixFilterWith LB.hPutStr LB.hGetContents


--------------------------------------------------------------------------------
-- | Overloaded compiler
unixFilterWith :: (Handle -> i -> IO ())  -- ^ Writer
               -> (Handle -> IO o)        -- ^ Reader
               -> String                  -- ^ Program name
               -> [String]                -- ^ Program args
               -> i                       -- ^ Program input
               -> Compiler o              -- ^ Program output
unixFilterWith writer reader programName args input = do
    debugCompiler ("Executing external program " ++ programName)
    unsafeCompiler $ unixFilterIO writer reader programName args input


--------------------------------------------------------------------------------
-- | Internally used function
unixFilterIO :: (Handle -> i -> IO ())
             -> (Handle -> IO o)
             -> String
             -> [String]
             -> i
             -> IO o
unixFilterIO writer reader programName args input = do
    -- Create pipes
    (stdinRead, stdinWrite) <- createPipe
    (stdoutRead, stdoutWrite) <- createPipe

    -- Fork the child
    _ <- forkProcess $ do
        -- Copy our pipes over the regular stdin/stdout
        _ <- dupTo stdinRead stdInput
        _ <- dupTo stdoutWrite stdOutput

        -- Close the now unneeded file descriptors in the child
        mapM_ closeFd [stdinWrite, stdoutRead, stdinRead, stdoutWrite]

        -- Execute the program
        _ <- executeFile programName True args Nothing
        return ()

    -- On the parent side, close the client-side FDs.
    mapM_ closeFd [stdinRead, stdoutWrite]

    -- Write the input to the child pipe
    _ <- forkIO $ do
        stdinWriteHandle <- fdToHandle stdinWrite
        writer stdinWriteHandle input
        hClose stdinWriteHandle

    -- Receive the output from the child
    stdoutReadHandle <- fdToHandle stdoutRead
    reader stdoutReadHandle
