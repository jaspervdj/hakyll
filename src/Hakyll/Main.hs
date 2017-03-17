--------------------------------------------------------------------------------
-- | Module providing the main hakyll function and command-line argument parsing
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Main
    ( hakyll
    , hakyllWith
    , hakyllWithExitCode
    ) where


--------------------------------------------------------------------------------
import           System.Environment              (getProgName)
import           System.IO.Unsafe                (unsafePerformIO)
import           System.Exit                     (ExitCode(ExitSuccess), exitWith)


--------------------------------------------------------------------------------
import           Data.Monoid                     ((<>))
import           Options.Applicative


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
hakyllWith conf rules = hakyllWithExitCode conf rules >>= exitWith

hakyllWithExitCode :: Config.Configuration -> Rules a -> IO ExitCode
hakyllWithExitCode conf rules = do
    args' <- customExecParser (prefs showHelpOnError) (info (helper <*> optionParser conf) (fullDesc <> progDesc (progName ++ " - Static site compiler created with Hakyll")))
    let args'' = optCommand args'

    let verbosity' = if verbosity args' then Logger.Debug else Logger.Message
        check'     =
            if internal_links args'' then Check.InternalLinks else Check.All

    logger <- Logger.new verbosity'

    case args'' of
        Build       -> Commands.build conf logger rules
        Check   _   -> Commands.check conf logger check'
        Clean       -> Commands.clean conf logger >> ok
        Deploy      -> Commands.deploy conf
        Preview p   -> Commands.preview conf logger rules p >> ok
        Rebuild     -> Commands.rebuild conf logger rules
        Server  _ _   -> Commands.server conf logger (host args'') (port args'') >> ok
        Watch   _ p s -> Commands.watch conf logger (host args'') p (not s) rules >> ok
    where
        ok = return ExitSuccess


--------------------------------------------------------------------------------

data Options = Options {verbosity :: Bool, optCommand :: Command}
    deriving (Show)

data Command
    = Build
    | Check   {internal_links :: Bool}
    | Clean
    | Deploy
    | Preview {port :: Int}
    | Rebuild
    | Server  {host :: String, port :: Int}
    | Watch   {host :: String, port :: Int, no_server :: Bool }
    deriving (Show)

optionParser :: Config.Configuration -> Parser Options
optionParser conf = Options <$> verboseParser <*> (commandParser conf)
    where
    verboseParser = switch (long "verbose" <> short 'v' <> help "Run in verbose mode")


commandParser :: Config.Configuration -> Parser Command
commandParser conf = subparser $ foldr ((<>) . produceCommand) mempty commands
    where
    produceCommand (a,b) = command a (info (helper <*> (fst b)) (snd b))
    portParser = option auto (long "port" <> help "Port to listen on" <> value (Config.previewPort conf))
    hostParser = strOption (long "host" <> help "Host to bind on" <> value (Config.previewHost conf))
    commands = [
        ("build",(pure Build,fullDesc <> progDesc "Generate the site")),
        ("check",(pure Check <*> switch (long "internal-links" <> help "Check internal links only"), fullDesc <> progDesc "Validate the site output")),
        ("clean",(pure Clean,fullDesc <> progDesc "Clean up and remove cache")),
        ("deploy",(pure Deploy,fullDesc <> progDesc "Upload/deploy your site")),
        ("preview",(pure Preview <*> portParser,fullDesc <> progDesc "[DEPRECATED] Please use the watch command")),
        ("rebuild",(pure Rebuild,fullDesc <> progDesc "Clean and build again")),
        ("server",(pure Server <*> hostParser <*> portParser,fullDesc <> progDesc "Start a preview server")),
        ("watch",(pure Watch <*> hostParser <*> portParser <*> switch (long "no-server" <> help "Disable the built-in web server"),fullDesc <> progDesc "Autocompile on changes and start a preview server.  You can watch and recompile without running a server with --no-server."))
        ]


--------------------------------------------------------------------------------
-- | This is necessary because not everyone calls their program the same...
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}
