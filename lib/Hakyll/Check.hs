--------------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Check
    ( Check (..)
    , check
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.MVar      (MVar, newEmptyMVar, putMVar,
                                               readMVar)
import           Control.Exception            (SomeAsyncException (..),
                                               SomeException (..), throw, try)
import           Control.Monad                (foldM, forM_)
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.State          (StateT, get, modify, runStateT)
import           Control.Monad.Trans          (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.List                    (isPrefixOf)
import qualified Data.Map.Lazy                as Map
import           Network.URI                  (unEscapeString)
import           System.Directory             (doesDirectoryExist,
                                               doesFileExist)
import           System.Exit                  (ExitCode (..))
import           System.FilePath              (takeDirectory, takeExtension,
                                               (</>))
import qualified Text.HTML.TagSoup            as TS


--------------------------------------------------------------------------------
#ifdef CHECK_EXTERNAL
import           Data.List                    (intercalate)
import           Data.Typeable                (cast)
import           Data.Version                 (versionBranch)
import           GHC.Exts                     (fromString)
import qualified Network.HTTP.Conduit         as Http
import qualified Network.HTTP.Types           as Http
import qualified Paths_hakyll                 as Paths_hakyll
#endif


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration
import           Hakyll.Core.Logger           (Logger)
import qualified Hakyll.Core.Logger           as Logger
import           Hakyll.Core.Util.File
import           Hakyll.Web.Html


--------------------------------------------------------------------------------
data Check = All | InternalLinks
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
check :: Configuration -> Logger -> Check -> IO ExitCode
check config logger check' = do
    ((), state) <- runChecker checkDestination config logger check'
    failed <- countFailedLinks state
    return $ if failed > 0 then ExitFailure 1 else ExitSuccess


--------------------------------------------------------------------------------
countFailedLinks :: CheckerState -> IO Int
countFailedLinks state = foldM addIfFailure 0 (Map.elems state)
    where addIfFailure failures mvar = do
              checkerWrite <- readMVar mvar
              return $ failures + checkerFaulty checkerWrite


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
type CheckerState = Map.Map URL (MVar CheckerWrite)


--------------------------------------------------------------------------------
type Checker a = ReaderT CheckerRead (StateT CheckerState IO) a


--------------------------------------------------------------------------------
type URL = String


--------------------------------------------------------------------------------
runChecker :: Checker a -> Configuration -> Logger -> Check
           -> IO (a, CheckerState)
runChecker checker config logger check' = do
    let read' = CheckerRead
                    { checkerConfig = config
                    , checkerLogger = logger
                    , checkerCheck  = check'
                    }
    Logger.flush logger
    runStateT (runReaderT checker read') Map.empty


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
        m <- liftIO newEmptyMVar
        checkUrlIfNeeded filePath (canonicalizeUrl url) m
    where
        -- Check scheme-relative links
        canonicalizeUrl url = if schemeRelative url then "http:" ++ url else url
        schemeRelative = isPrefixOf "//"


--------------------------------------------------------------------------------
checkUrlIfNeeded :: FilePath -> URL -> MVar CheckerWrite -> Checker ()
checkUrlIfNeeded filepath url m = do
    logger     <- checkerLogger           <$> ask
    needsCheck <- (== All) . checkerCheck <$> ask
    checked    <- (url `Map.member`)      <$> get
    if not needsCheck || checked
        then Logger.debug logger "Already checked, skipping"
        else do modify $ Map.insert url m
                checkUrl filepath url


--------------------------------------------------------------------------------
checkUrl :: FilePath -> URL -> Checker ()
checkUrl filePath url
    | isExternal url  = checkExternalUrl url
    | hasProtocol url = skip url $ Just "Unknown protocol, skipping"
    | otherwise       = checkInternalUrl filePath url
  where
    validProtoChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+-."
    hasProtocol str = case break (== ':') str of
        (proto, ':' : _) -> all (`elem` validProtoChars) proto
        _                -> False


--------------------------------------------------------------------------------
ok :: URL -> Checker ()
ok url = putCheckResult url mempty {checkerOk = 1}


--------------------------------------------------------------------------------
skip :: URL -> Maybe String -> Checker ()
skip url maybeReason = do
    logger <- checkerLogger <$> ask
    case maybeReason of
        Nothing     -> return ()
        Just reason -> Logger.debug logger reason
    putCheckResult url mempty {checkerOk = 1}


--------------------------------------------------------------------------------
faulty :: URL -> Maybe String -> Checker ()
faulty url reason = do
    logger <- checkerLogger <$> ask
    Logger.error logger $ "Broken link to " ++ show url ++ explanation
    putCheckResult url mempty {checkerFaulty = 1}
  where
    formatExplanation = (" (" ++) . (++ ")")
    explanation = maybe "" formatExplanation reason


--------------------------------------------------------------------------------
putCheckResult :: URL -> CheckerWrite -> Checker ()
putCheckResult url result = do
    state <- get
    let maybeMVar = Map.lookup url state
    case maybeMVar of
        Just m -> liftIO $ putMVar m result
        Nothing -> do
            logger <- checkerLogger <$> ask
            Logger.debug logger "Failed to find existing entry for checked URL"


--------------------------------------------------------------------------------
checkInternalUrl :: FilePath -> URL -> Checker ()
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
        if exists then ok url else faulty url Nothing
  where
    url' = stripFragments $ unEscapeString url


--------------------------------------------------------------------------------
checkExternalUrl :: URL -> Checker ()
#ifdef CHECK_EXTERNAL
checkExternalUrl url = do
    result <- requestExternalUrl url
    case result of
        Left (SomeException e) ->
            case (cast e :: Maybe SomeAsyncException) of
                Just ae -> throw ae
                _       -> faulty url (Just $ showException e)
        Right _ -> ok url
    where
        -- Convert exception to a concise form
        showException e = case cast e of
            Just (Http.HttpExceptionRequest _ e') -> show e'
            _                                     -> head $ words $ show e

requestExternalUrl :: URL -> Checker (Either SomeException Bool)
requestExternalUrl url = liftIO $ try $ do
    mgr <- Http.newManager Http.tlsManagerSettings
    runResourceT $ do
        request  <- Http.parseRequest url
        response <- Http.http (settings request) mgr
        let code = Http.statusCode (Http.responseStatus response)
        return $ code >= 200 && code < 300
    where
        -- Add additional request info
        settings r = r
            { Http.method         = "HEAD"
            , Http.redirectCount  = 10
            , Http.requestHeaders = ("User-Agent", ua) : Http.requestHeaders r
            }

        -- Nice user agent info
        ua = fromString $ "hakyll-check/" ++
             (intercalate "." $ map show $ versionBranch Paths_hakyll.version)
#else
checkExternalUrl url = skip url Nothing
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
