--------------------------------------------------------------------------------
module Hakyll.Check
    ( runCheck
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Monad             (forM_)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Writer      (WriterT, runWriterT, tell)
import           Data.List                 (isPrefixOf)
import           Data.Monoid               (Monoid (..))
import           System.Directory          (doesDirectoryExist, doesFileExist)
import           System.FilePath           (takeDirectory, takeExtension, (</>))
import qualified Text.HTML.TagSoup         as TS


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration
import           Hakyll.Core.Logger        (Logger)
import qualified Hakyll.Core.Logger        as Logger
import           Hakyll.Core.Util.File
import           Hakyll.Web.Html


--------------------------------------------------------------------------------
data CheckerRead = CheckerRead
    { checkerConfig :: Configuration
    , checkerLogger :: Logger
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
type Checker a = ReaderT CheckerRead (WriterT CheckerWrite IO) a


--------------------------------------------------------------------------------
runCheck :: Configuration -> IO ()
runCheck config = do
    logger <- Logger.new (verbosity config)
    let read' = CheckerRead config logger
    ((), write) <- runWriterT $ runReaderT check read'
    Logger.header logger $ show write
    Logger.flush logger


--------------------------------------------------------------------------------
check :: Checker ()
check = do
    config <- checkerConfig <$> ask
    files  <- liftIO $ getRecursiveContents (destinationDirectory config)

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
    Logger.header logger $ "Checking " ++ filePath

    let tags = TS.parseTags contents
        -- Lots of logic here...
        urls = filter (not . null) $
                map stripFragments $
                filter (not . isExternal) $
                getUrls tags

    mapM_ (checkUrl filePath) urls


--------------------------------------------------------------------------------
checkUrl :: FilePath -> String -> Checker ()
checkUrl base url = do
    logger <- checkerLogger <$> ask
    config <- checkerConfig <$> ask

    let dest = destinationDirectory config
        dir  = takeDirectory base
        filePath
            | "/" `isPrefixOf` url = dest ++ url
            | otherwise            = dir </> url

    exists <- checkFileExists filePath
    if exists
        then tell $ mempty {checkerOk = 1}
        else do
            tell $ mempty {checkerFaulty = 1}
            Logger.error logger $ base ++ ": broken reference to " ++ show url


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
