--------------------------------------------------------------------------------
-- | Module providing the main hakyll function and command-line argument parsing
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Main
    ( hakyll
    , hakyllWith
    ) where


--------------------------------------------------------------------------------
import           System.Console.CmdArgs
import qualified System.Console.CmdArgs.Explicit as CA
import           System.Environment              (getProgName)
import           System.IO.Unsafe                (unsafePerformIO)


--------------------------------------------------------------------------------
import qualified Hakyll.Check                    as Check
import qualified Hakyll.Commands                 as Commands
import qualified Hakyll.Core.Configuration       as Config
import qualified Hakyll.Core.Logger              as Logger
import           Hakyll.Core.Rules


--------------------------------------------------------------------------------
-- | This usualy is the function with which the user runs the hakyll compiler
hakyll :: Rules a -> IO ()
hakyll = hakyllWith Config.defaultConfiguration


--------------------------------------------------------------------------------
-- | A variant of 'hakyll' which allows the user to specify a custom
-- configuration
hakyllWith :: Config.Configuration -> Rules a -> IO ()
hakyllWith conf rules = do
    args' <- cmdArgs hakyllArgs

    let verbosity' = if verbose args' then Logger.Debug else Logger.Message
        check'     =
            if internal_links args' then Check.InternalLinks else Check.All

    case args' of
        Build   _   -> Commands.build conf verbosity' rules
        Check   _ _ -> Commands.check conf verbosity' check'
        Clean   _   -> Commands.clean conf
        Deploy  _   -> Commands.deploy conf
        Help    _   -> showHelp
        Preview _ p -> Commands.preview conf verbosity' rules p
        Rebuild _   -> Commands.rebuild conf verbosity' rules
        Server  _ _ -> Commands.server conf (port args')


--------------------------------------------------------------------------------
-- | Show usage information.
showHelp :: IO ()
showHelp = print $ CA.helpText [] CA.HelpFormatOne $ cmdArgsMode hakyllArgs


--------------------------------------------------------------------------------
data HakyllArgs
    = Build   {verbose :: Bool}
    | Check   {verbose :: Bool, internal_links :: Bool}
    | Clean   {verbose :: Bool}
    | Deploy  {verbose :: Bool}
    | Help    {verbose :: Bool}
    | Preview {verbose :: Bool, port :: Int}
    | Rebuild {verbose :: Bool}
    | Server  {verbose :: Bool, port :: Int}
    deriving (Data, Typeable, Show)


--------------------------------------------------------------------------------
hakyllArgs :: HakyllArgs
hakyllArgs = modes
    [ (Build $ verboseFlag def) &= help "Generate the site"
    , (Check (verboseFlag def) (False &= help "Check internal links only")) &=
        help "Validate the site output"
    , (Clean $ verboseFlag def) &= help "Clean up and remove cache"
    , (Deploy $ verboseFlag def) &= help "Upload/deploy your site"
    , (Help $ verboseFlag def) &= help "Show this message" &= auto
    , (Preview (verboseFlag def) (portFlag 8000)) &=
        help "Start a preview server and autocompile on changes"
    , (Rebuild $ verboseFlag def) &= help "Clean and build again"
    , (Server (verboseFlag def) (portFlag 8000)) &=
        help "Start a preview server"
    ] &= help "Hakyll static site compiler" &= program progName


--------------------------------------------------------------------------------
verboseFlag :: Data a => a -> a
verboseFlag x = x &= help "Run in verbose mode"
{-# INLINE verboseFlag #-}


--------------------------------------------------------------------------------
portFlag :: Data a => a -> a
portFlag x = x &= help "Port to listen on"
{-# INLINE portFlag #-}


--------------------------------------------------------------------------------
-- | This is necessary because not everyone calls their program the same...
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}
