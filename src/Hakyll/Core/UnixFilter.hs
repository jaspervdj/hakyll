-- | A Compiler that supports unix filters.
--
module Hakyll.Core.UnixFilter
    ( unixFilter
    ) where

import Control.Concurrent (forkIO)
import System.IO (hPutStr, hClose, hGetContents)
import System.Posix.Process (executeFile, forkProcess)
import System.Posix.IO ( dupTo, createPipe, stdInput
                       , stdOutput, closeFd, fdToHandle
                       )

import Hakyll.Core.Compiler

-- | Use a unix filter as compiler. For example, we could use the 'rev' program
-- as a compiler.
--
-- > rev :: Compiler Resource String
-- > rev = getResourceString >>> unixFilter "rev" []
--
-- A more realistic example: one can use this to call, for example, the sass
-- compiler on CSS files. More information about sass can be found here:
--
-- <http://sass-lang.com/>
--
-- The code is fairly straightforward, given that we use @.scss@ for sass:
--
-- > route   "style.scss" $ setExtension "css"
-- > compile "style.scss" $
-- >     getResourceString >>> unixFilter "sass" ["-s", "--scss"]
-- >                       >>> arr compressCss
--
unixFilter :: String                  -- ^ Program name
           -> [String]                -- ^ Program args
           -> Compiler String String  -- ^ Resulting compiler
unixFilter programName args = unsafeCompiler $ \input -> do
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
        hPutStr stdinWriteHandle input
        hClose stdinWriteHandle

    -- Receive the output from the child
    stdoutReadHandle <- fdToHandle stdoutRead
    hGetContents stdoutReadHandle
