--------------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Check
    ( Check (..)
    , check
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Monad             (forM_)
import           Control.Monad.Reader      (ask)
import           Control.Monad.RWS         (RWST, runRWST)
import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Writer      (tell)
import           Data.List                 (isPrefixOf)
import           Data.Monoid               (Monoid (..))
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Network.URI               (unEscapeString)
import           System.Directory          (doesDirectoryExist, doesFileExist)
import           System.Exit               (ExitCode (..))
import           System.FilePath           (takeDirectory, takeExtension, (</>))
import qualified Text.HTML.TagSoup         as TS


--------------------------------------------------------------------------------
#ifdef CHECK_EXTERNAL
import           Control.Exception         (AsyncException (..),
                                            SomeException (..), handle, throw)
import           Control.Monad.State       (get, modify)
import           Data.List                 (intercalate)
import           Data.Typeable             (cast)
import           Data.Version              (versionBranch)
import           GHC.Exts                  (fromString)
import qualified Network.HTTP.Conduit      as Http
import qualified Network.HTTP.Types        as Http
import qualified Paths_hakyll              as Paths_hakyll
#endif


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration
import           Hakyll.Core.Logger        (Logger)
import qualified Hakyll.Core.Logger        as Logger
import           Hakyll.Core.Util.File
import           Hakyll.Web.Html


--------------------------------------------------------------------------------
data Check = All | InternalLinks
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
check :: Configuration -> Logger -> Check -> IO ExitCode
check config logger check' = do
    ((), write) <- runChecker checkDestination config logger check'
    return $ if checkerFaulty write > 0 then ExitFailure 1 else ExitSuccess


--------------------------------------------------------------------------------
data CheckerRead = CheckerRead
    { checkerConfig :: Configuration
    , checkerLogger :: Logger
    , checkerCheck  :: Check
    }


--------------------------------------------------------------------------------
data CheckerWrite = CheckerWrite
    { checkerFaulty :: Int
    , checkerOk     :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
instance Monoid CheckerWrite where
    mempty                                            = CheckerWrite 0 0
    mappend (CheckerWrite f1 o1) (CheckerWrite f2 o2) =
        CheckerWrite (f1 + f2) (o1 + o2)


--------------------------------------------------------------------------------
type CheckerState = Set String


--------------------------------------------------------------------------------
type Checker a = RWST CheckerRead CheckerWrite CheckerState IO a


--------------------------------------------------------------------------------
runChecker :: Checker a -> Configuration -> Logger -> Check
           -> IO (a, CheckerWrite)
runChecker checker config logger check' = do
    let read' = CheckerRead
                    { checkerConfig = config
                    , checkerLogger = logger
                    , checkerCheck  = check'
                    }

    (x, _, write) <- runRWST checker read' S.empty
    Logger.flush logger
    return (x, write)


--------------------------------------------------------------------------------
checkDestination :: Checker ()
checkDestination = do
    config <- checkerConfig <$> ask
    files  <- liftIO $ getRecursiveContents
        (const $ return False) (destinationDirectory config)

    let htmls =
            [ destinationDirectory config </> file
            | file <- files
            , takeExtension file == ".html"
            ]

    forM_ htmls checkFile


--------------------------------------------------------------------------------
checkFile :: FilePath -> Checker ()
checkFile filePath = do
    logger   <- checkerLogger <$> ask
    contents <- liftIO $ readFile filePath
    Logger.header logger $ "Checking file " ++ filePath

    let urls = getUrls $ TS.parseTags contents
    forM_ urls $ \url -> do
        Logger.debug logger $ "Checking link " ++ url
        checkUrl filePath url


--------------------------------------------------------------------------------
checkUrl :: FilePath -> String -> Checker ()
checkUrl filePath url
    | isExternal url  = checkExternalUrl url
    | hasProtocol url = skip "Unknown protocol, skipping"
    | otherwise       = checkInternalUrl filePath url
  where
    validProtoChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+-."
    hasProtocol str = case break (== ':') str of
        (proto, ':' : _) -> all (`elem` validProtoChars) proto
        _                -> False


--------------------------------------------------------------------------------
ok :: String -> Checker ()
ok _ = tell $ mempty {checkerOk = 1}


--------------------------------------------------------------------------------
skip :: String -> Checker ()
skip reason = do
    logger <- checkerLogger <$> ask
    Logger.debug logger $ reason
    tell $ mempty {checkerOk = 1}

--------------------------------------------------------------------------------
faulty :: String -> Checker ()
faulty url = do
    logger <- checkerLogger <$> ask
    Logger.error logger $ "Broken link to " ++ show url
    tell $ mempty {checkerFaulty = 1}


--------------------------------------------------------------------------------
checkInternalUrl :: FilePath -> String -> Checker ()
checkInternalUrl base url = case url' of
    "" -> ok url
    _  -> do
        config <- checkerConfig <$> ask
        let dest = destinationDirectory config
            dir  = takeDirectory base
            filePath
                | "/" `isPrefixOf` url' = dest ++ url'
                | otherwise             = dir </> url'

        exists <- checkFileExists filePath
        if exists then ok url else faulty url
  where
    url' = stripFragments $ unEscapeString url


--------------------------------------------------------------------------------
checkExternalUrl :: String -> Checker ()
#ifdef CHECK_EXTERNAL
checkExternalUrl url = do
    logger     <- checkerLogger           <$> ask
    needsCheck <- (== All) . checkerCheck <$> ask
    checked    <- (url `S.member`)        <$> get

    if not needsCheck || checked
        then Logger.debug logger "Already checked, skipping"
        else do
            isOk <- liftIO $ handle (failure logger) $
                Http.withManager $ \mgr -> do
                    request  <- Http.parseUrl urlToCheck
                    response <- Http.http (settings request) mgr
                    let code = Http.statusCode (Http.responseStatus response)
                    return $ code >= 200 && code < 300

            modify $ if schemeRelative url
                         then S.insert urlToCheck . S.insert url
                         else S.insert url
            if isOk then ok url else faulty url
  where
    -- Add additional request info
    settings r = r
        { Http.method         = "HEAD"
        , Http.redirectCount  = 10
        , Http.requestHeaders = ("User-Agent", ua) : Http.requestHeaders r
        }

    -- Nice user agent info
    ua = fromString $ "hakyll-check/" ++
        (intercalate "." $ map show $ versionBranch $ Paths_hakyll.version)

    -- Catch all the things except UserInterrupt
    failure logger (SomeException e) = case cast e of
        Just UserInterrupt -> throw UserInterrupt
        _                  -> Logger.error logger (show e) >> return False

    -- Check scheme-relative links
    schemeRelative = isPrefixOf "//"
    urlToCheck     = if schemeRelative url then "http:" ++ url else url
#else
checkExternalUrl _ = return ()
#endif



--------------------------------------------------------------------------------
-- | Wraps doesFileExist, also checks for index.html
checkFileExists :: FilePath -> Checker Bool
checkFileExists filePath = liftIO $ do
    file <- doesFileExist filePath
    dir  <- doesDirectoryExist filePath
    case (file, dir) of
        (True, _) -> return True
        (_, True) -> doesFileExist $ filePath </> "index.html"
        _         -> return False


--------------------------------------------------------------------------------
stripFragments :: String -> String
stripFragments = takeWhile (not . flip elem ['?', '#'])
