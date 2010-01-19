-- | Module containing a small, simple http file server for testing and preview
--   purposes.
module Network.Hakyll.SimpleServer
    ( simpleServer
    ) where

import Prelude hiding (log)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)
import Network
import System.IO
import System.Directory (doesFileExist, doesDirectoryExist)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import System.FilePath (takeExtension)
import qualified Data.Map as M
import Data.List (intercalate)

import Text.Hakyll.Util
import Text.Hakyll.Regex

-- | Function to log from a chan.
log :: Chan String -> IO ()
log logChan = forever (readChan logChan >>= hPutStrLn stderr)

-- | General server configuration.
data ServerConfig = ServerConfig { documentRoot :: FilePath
                                 , portNumber :: PortNumber
                                 , logChannel :: Chan String
                                 }

-- | Custom monad stack.
type Server = ReaderT ServerConfig IO

-- | Simple representation of a HTTP request.
data Request = Request { requestMethod :: String
                       , requestURI :: String
                       , requestVersion :: String
                       } deriving (Ord, Eq)

instance Show Request where
    show request =  requestMethod request ++ " "
                 ++ requestURI request ++ " "
                 ++ requestVersion request

-- | Read a HTTP request from a 'Handle'. For now, this will ignore the request
--   headers and body.
readRequest :: Handle -> Server Request
readRequest handle = do
    requestLine <- liftIO $ hGetLine handle
    let [method, uri, version] = map trim $ splitRegex " " requestLine
        request = Request { requestMethod = method
                          , requestURI = uri
                          , requestVersion = version
                          }
    return request

-- | Simple representation of the HTTP response we send back.
data Response = Response { responseVersion :: String
                         , responseStatusCode :: Int
                         , responsePhrase :: String
                         , responseHeaders :: M.Map String String
                         , responseBody :: String
                         } deriving (Ord, Eq)

instance Show Response where
    show response =  responseVersion response ++ " "
                  ++ show (responseStatusCode response) ++ " "
                  ++ responsePhrase response

-- | A default response.
defaultResponse :: Response
defaultResponse = Response { responseVersion = "HTTP/1.1"
                           , responseStatusCode = 0
                           , responsePhrase = ""
                           , responseHeaders = M.empty
                           , responseBody = ""
                           }

-- | Create a response for a given HTTP request.
createResponse :: Request -> Server Response
createResponse request
    | requestMethod request == "GET" = createGetResponse request
    | otherwise = return $ createErrorResponse 501 "Not Implemented"

-- | Create a simple error response.
createErrorResponse :: Int      -- ^ Error code.
                    -> String   -- ^ Error phrase.
                    -> Response -- ^ Result.
createErrorResponse statusCode phrase = defaultResponse
    { responseStatusCode = statusCode
    , responsePhrase = phrase
    , responseHeaders = M.singleton "Content-Type" "text/html"
    , responseBody =
           "<html> <head> <title>" ++ show statusCode ++ "</title> </head>"
        ++ "<body> <h1>" ++ show statusCode ++ "</h1>\n"
        ++ "<p>" ++ phrase ++ "</p> </body> </html>"
    }

-- | Create a simple get response.
createGetResponse :: Request -> Server Response
createGetResponse request = do
    -- Construct the complete fileName of the requested resource.
    config <- ask
    let uri = requestURI request
        log' = writeChan (logChannel config)
    isDirectory <- liftIO $ doesDirectoryExist $ documentRoot config ++ uri
    let fileName =
            documentRoot config ++ if isDirectory then uri ++ "/index.html"
                                                  else uri

        create200 = do
            h <- openBinaryFile fileName ReadMode
            contentLength <- hFileSize h
            body <- hGetContents h
            let mimeHeader = getMIMEHeader fileName
                headers = ("Content-Length", show contentLength) : mimeHeader
            return $ defaultResponse
                { responseStatusCode = 200
                , responsePhrase = "OK"
                , responseHeaders = responseHeaders defaultResponse
                    `M.union` M.fromList headers
                , responseBody = body
                }

        -- Called when an error occurs during the creation of a 200 response.
        create500 e = do
            log' $ "Internal Error: " ++ show e
            return $ createErrorResponse 500 "Internal Server Error"

    -- Send back the page if found.
    exists <- liftIO $ doesFileExist fileName
    if exists
        then liftIO $ catch create200 create500
        else do liftIO $ log' $ "Not Found: " ++ fileName
                return $ createErrorResponse 404 "Not Found"

-- | Get the mime header for a certain filename. This is based on the extension
--   of the given 'FilePath'.
getMIMEHeader :: FilePath -> [(String, String)]
getMIMEHeader fileName =
    case result of (Just x) -> [("Content-Type", x)]
                   Nothing  -> []
  where
    result = lookup (takeExtension fileName) [ (".css", "text/css")
                                               , (".gif", "image/gif")
                                               , (".htm", "text/html")
                                               , (".html", "text/html")
                                               , (".jpeg", "image/jpeg")
                                               , (".jpg", "image/jpeg")
                                               , (".js", "text/javascript")
                                               , (".png", "image/png")
                                               , (".txt", "text/plain")
                                               , (".xml", "text/xml")
                                               ]

-- | Respond to an incoming request.
respond :: Handle -> Server ()
respond handle = do
    -- Read the request and create a response.
    request <- readRequest handle
    response <- createResponse request

    -- Generate some output.
    config <- ask
    liftIO $ writeChan (logChannel config)
           $ show request ++ " => " ++ show response

    -- Send the response back to the handle.
    liftIO $ putResponse response
  where
    putResponse response = do hPutStr handle $ intercalate " "
                                  [ responseVersion response
                                  , show $ responseStatusCode response
                                  , responsePhrase response
                                  ]
                              hPutStr handle "\r\n"
                              mapM_ putHeader
                                    (M.toList $ responseHeaders response)
                              hPutStr handle "\r\n"
                              hPutStr handle $ responseBody response
                              hPutStr handle "\r\n"
                              hClose handle

    putHeader (key, value) =
        hPutStr handle $ key ++ ": " ++ value ++ "\r\n"

-- | Start a simple http server on the given 'PortNumber', serving the given
--   directory.
simpleServer :: PortNumber -> FilePath -> IO ()
simpleServer port root = do
    -- Channel to send logs to
    logChan <- newChan

    let config = ServerConfig { documentRoot = root
                              , portNumber = port
                              , logChannel = logChan
                              }

          -- When a client connects, respond in a separate thread.
        listen socket = do (handle, _, _) <- accept socket
                           forkIO (runReaderT (respond handle) config)

    -- Handle logging in a separate thread
    _ <- forkIO (log logChan)

    writeChan logChan $ "Starting hakyll server on port " ++ show port ++ "..."
    socket <- listenOn (PortNumber port)
    forever (listen socket)
