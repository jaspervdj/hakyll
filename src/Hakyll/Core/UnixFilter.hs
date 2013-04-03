--------------------------------------------------------------------------------
-- | A Compiler that supports unix filters.
module Hakyll.Core.UnixFilter
    ( unixFilter
    , unixFilterLBS
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.DeepSeq         (deepseq)
import           Control.Monad           (forM_)
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as LB
import           Data.IORef              (newIORef, readIORef, writeIORef)
import           Data.Monoid             (Monoid, mempty)
import           System.Exit             (ExitCode (..))
import           System.IO               (Handle, hClose, hFlush, hGetContents,
                                          hPutStr, hSetEncoding, localeEncoding)
import           System.Process


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
        out <- hGetContents handle
        deepseq out (return out)


--------------------------------------------------------------------------------
-- | Variant of 'unixFilter' that should be used for binary files
--
-- > match "music.wav" $ do
-- >     route   $ setExtension "ogg"
-- >     compile $ getResourceLBS >>= withItemBody (unixFilterLBS "oggenc" ["-"])
unixFilterLBS :: String               -- ^ Program name
              -> [String]             -- ^ Program args
              -> ByteString           -- ^ Program input
              -> Compiler ByteString  -- ^ Program output
unixFilterLBS = unixFilterWith LB.hPutStr $ \handle -> do
    out <- LB.hGetContents handle
    LB.length out `seq` return out


--------------------------------------------------------------------------------
-- | Overloaded compiler
unixFilterWith :: Monoid o
               => (Handle -> i -> IO ())  -- ^ Writer
               -> (Handle -> IO o)        -- ^ Reader
               -> String                  -- ^ Program name
               -> [String]                -- ^ Program args
               -> i                       -- ^ Program input
               -> Compiler o              -- ^ Program output
unixFilterWith writer reader programName args input = do
    debugCompiler ("Executing external program " ++ programName)
    (output, err, exitCode) <- unsafeCompiler $
        unixFilterIO writer reader programName args input
    forM_ (lines err) debugCompiler
    case exitCode of
        ExitSuccess   -> return output
        ExitFailure e -> fail $
            "Hakyll.Core.UnixFilter.unixFilterWith: " ++
            unwords (programName : args) ++ " gave exit code " ++ show e


--------------------------------------------------------------------------------
-- | Internally used function
unixFilterIO :: Monoid o
             => (Handle -> i -> IO ())
             -> (Handle -> IO o)
             -> String
             -> [String]
             -> i
             -> IO (o, String, ExitCode)
unixFilterIO writer reader programName args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc programName args)
                { std_in  = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }

    -- Create boxes
    lock   <- newEmptyMVar
    outRef <- newIORef mempty
    errRef <- newIORef ""

    -- Write the input to the child pipe
    _ <- forkIO $ writer inh input >> hFlush inh >> hClose inh

    -- Read from stdout
    _ <- forkIO $ do
        out <- reader outh
        hClose outh
        writeIORef outRef out
        putMVar lock ()

    -- Read from stderr
    _ <- forkIO $ do
        err <- hGetContents errh
        hClose errh
        writeIORef errRef err
        putMVar lock ()

    -- Get exit code & return
    takeMVar lock
    takeMVar lock
    exitCode <- waitForProcess pid
    out      <- readIORef outRef
    err      <- readIORef errRef
    return (out, err, exitCode)
