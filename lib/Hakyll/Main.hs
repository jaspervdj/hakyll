--------------------------------------------------------------------------------
-- | Module providing the main hakyll function and command-line argument parsing
{-# LANGUAGE CPP        #-}
{-# LANGUAGE Rank2Types #-}

module Hakyll.Main
    ( hakyll
    , parHakyll
    , hakyllWith
    , parHakyllWith
    , hakyllWithArgs
    , parHakyllWithArgs
    , hakyllWithExitCode
    , parHakyllWithExitCode
    , hakyllWithExitCodeAndArgs
    , parHakyllWithExitCodeAndArgs
    , Options(..)
    , Command(..)
    ) where


--------------------------------------------------------------------------------
import           System.Environment        (getProgName)
import           System.Exit               (ExitCode (ExitSuccess), exitWith)
import           System.IO.Unsafe          (unsafePerformIO)


--------------------------------------------------------------------------------
import           Data.Monoid               ((<>))
import qualified Options.Applicative       as OA


--------------------------------------------------------------------------------
import qualified Hakyll.Check              as Check
import qualified Hakyll.Commands           as Commands
import qualified Hakyll.Core.Configuration as Config
import qualified Hakyll.Core.Logger        as Logger
import           Hakyll.Core.Rules


--------------------------------------------------------------------------------
-- | This usually is the function with which the user runs the hakyll compiler
hakyll :: Rules a -> IO ()
hakyll rules = parHakyll sequence [rules]

-- | This usually is the function with which the user runs the hakyll compiler
parHakyll :: (forall b. [IO b] -> IO [b]) -> [Rules a] -> IO ()
parHakyll ev rules = parHakyllWith ev Config.defaultConfiguration rules

--------------------------------------------------------------------------------
-- | A variant of 'hakyll' which allows the user to specify a custom
-- configuration
hakyllWith :: Config.Configuration -> Rules a -> IO ()
hakyllWith conf rules = parHakyllWith sequence conf [rules]

-- | A variant of 'parHakyll' which allows the user to specify a custom
-- configuration
parHakyllWith :: (forall b. [IO b] -> IO [b])
              -> Config.Configuration
              -> [Rules a]
              -> IO ()
parHakyllWith ev conf rules = parHakyllWithExitCode ev conf rules >>= exitWith

--------------------------------------------------------------------------------
-- | A variant of 'hakyll' which returns an 'ExitCode'
hakyllWithExitCode :: Config.Configuration -> Rules a -> IO ExitCode
hakyllWithExitCode conf rules = parHakyllWithExitCode sequence conf [rules]

-- | A variant of 'parHakyll' which returns an 'ExitCode'
parHakyllWithExitCode :: (forall b. [IO b] -> IO [b])
                      -> Config.Configuration
                      -> [Rules a]
                      -> IO ExitCode
parHakyllWithExitCode ev conf rules =  do
    args <- defaultParser conf
    parHakyllWithExitCodeAndArgs ev conf args rules

--------------------------------------------------------------------------------
-- | A variant of 'hakyll' which expects a 'Configuration' and command-line
-- 'Options'. This gives freedom to implement your own parsing.
hakyllWithArgs :: Config.Configuration -> Options -> Rules a -> IO ()
hakyllWithArgs conf args rules = parHakyllWithArgs sequence conf args [rules]

-- | A variant of 'parHakyll' which expects a 'Configuration' and command-line
-- 'Options'. This gives freedom to implement your own parsing.
parHakyllWithArgs :: (forall b. [IO b] -> IO [b])
                  -> Config.Configuration
                  -> Options
                  -> [Rules a]
                  -> IO ()
parHakyllWithArgs ev conf args rules =
    parHakyllWithExitCodeAndArgs ev conf args rules >>= exitWith

--------------------------------------------------------------------------------
-- | A variant of 'parHakyll' which expects a 'Configuration' and command-line
-- 'Options'. This gives freedom to implement your own parsing.
hakyllWithExitCodeAndArgs :: Config.Configuration ->
                              Options -> Rules a -> IO ExitCode
hakyllWithExitCodeAndArgs conf args rules =
  parHakyllWithExitCodeAndArgs sequence conf args [rules]

-- | A variant of 'parHakyll' which expects a 'Configuration' and command-line
-- 'Options'. This gives freedom to implement your own parsing.
parHakyllWithExitCodeAndArgs :: (forall b. [IO b] -> IO [b])
                             -> Config.Configuration
                             -> Options
                             -> [Rules a]
                             -> IO ExitCode
parHakyllWithExitCodeAndArgs ev conf args rules = do
    let args' = optCommand args
        verbosity' = if verbosity args then Logger.Debug else Logger.Message
        check     =
            if internal_links args' then Check.InternalLinks else Check.All

    logger <- Logger.new verbosity'
    invokeCommands ev args' conf check logger rules
--------------------------------------------------------------------------------
defaultParser :: Config.Configuration -> IO Options
defaultParser conf =
    OA.customExecParser (OA.prefs OA.showHelpOnError)
        (OA.info (OA.helper <*> optionParser conf)
        (OA.fullDesc <> OA.progDesc
        (progName ++ " - Static site compiler created with Hakyll")))


--------------------------------------------------------------------------------
invokeCommands :: (forall b. [IO b] -> IO [b])
               -> Command
               -> Config.Configuration
               -> Check.Check
               -> Logger.Logger
               -> [Rules a]
               -> IO ExitCode
invokeCommands ev args conf check logger rules =
    case args of
        Build          -> Commands.buildPar ev conf logger rules
        Check   _      -> Commands.check conf logger check
        Clean          -> Commands.clean conf logger >> ok
        Deploy         -> Commands.deploy conf
        Preview p      -> Commands.preview conf logger (head rules) p >> ok
        Rebuild        -> Commands.rebuildPar ev conf logger rules
        Server  _ _    -> Commands.server conf logger (host args) (port args) >> ok
        Watch   _ p s  -> Commands.watchPar ev conf logger (host args) p (not s) rules >> ok
    where
        ok = return ExitSuccess


--------------------------------------------------------------------------------

-- | The parsed command-line options.
data Options = Options {verbosity :: Bool, optCommand :: Command}
    deriving (Show)

-- | The command to run.
data Command
    = Build
    -- ^ Generate the site.
    | Check   {internal_links :: Bool}
    -- ^ Validate the site output.
    | Clean
    -- ^ Clean up and remove cache.
    | Deploy
    -- ^ Upload/deploy your site.
    | Preview {port :: Int}
    -- ^ [DEPRECATED] Please use the watch command.
    | Rebuild
    -- ^ Clean and build again.
    | Server  {host :: String, port :: Int}
    -- ^ Start a preview server.
    | Watch   {host :: String, port :: Int, no_server :: Bool }
    -- ^ Autocompile on changes and start a preview server.
    deriving (Show)

{-# DEPRECATED Preview "Use Watch instead." #-}

optionParser :: Config.Configuration -> OA.Parser Options
optionParser conf = Options <$> verboseParser <*> commandParser conf
    where
    verboseParser = OA.switch (OA.long "verbose" <> OA.short 'v' <> OA.help "Run in verbose mode")


commandParser :: Config.Configuration -> OA.Parser Command
commandParser conf = OA.subparser $ foldr ((<>) . produceCommand) mempty commands
    where
    portParser = OA.option OA.auto (OA.long "port" <> OA.help "Port to listen on" <> OA.value (Config.previewPort conf))
    hostParser = OA.strOption (OA.long "host" <> OA.help "Host to bind on" <> OA.value (Config.previewHost conf))

    produceCommand (c,a,b) = OA.command c (OA.info (OA.helper <*> a) (b))

    commands =
        [ ( "build"
          , pure Build
          , OA.fullDesc <> OA.progDesc "Generate the site"
          )
        , ( "check"
          , pure Check <*> OA.switch (OA.long "internal-links" <> OA.help "Check internal links only")
          , OA.fullDesc <> OA.progDesc "Validate the site output"
          )
        , ( "clean"
          , pure Clean
          , OA.fullDesc <> OA.progDesc "Clean up and remove cache"
          )
        , ( "deploy"
          , pure Deploy
          , OA.fullDesc <> OA.progDesc "Upload/deploy your site"
           )
        , ( "preview"
          , pure Preview <*> portParser
          , OA.fullDesc <> OA.progDesc "[DEPRECATED] Please use the watch command"
          )
        , ( "rebuild"
          , pure Rebuild
          , OA.fullDesc <> OA.progDesc "Clean and build again"
          )
        , ( "server"
          , pure Server <*> hostParser <*> portParser
          , OA.fullDesc <> OA.progDesc "Start a preview server"
          )
        , ( "watch"
          , pure Watch <*> hostParser <*> portParser <*> OA.switch (OA.long "no-server" <> OA.help "Disable the built-in web server")
          , OA.fullDesc <> OA.progDesc "Autocompile on changes and start a preview server.  You can watch and recompile without running a server with --no-server."
          )
        ]


--------------------------------------------------------------------------------
-- | This is necessary because not everyone calls their program the same...
progName :: String
progName = unsafePerformIO getProgName
{-# NOINLINE progName #-}
