-- | Module containing a small, simple http file server for testing and preview
--   purposes.
module Network.Hakyll.SimpleServer
    ( simpleServer
    ) where

import Prelude hiding (log)
import Network
import Control.Monad (forever, mapM_)
import Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)
import System.IO (Handle, hClose, hGetLine, hPutStr, hPutStrLn, stderr)
import System.Directory (doesFileExist, doesDirectoryExist)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import System.FilePath (takeExtension)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Text.Hakyll.Util

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
data Request = Request { requestMethod :: B.ByteString
                       , requestURI :: B.ByteString
                       , requestVersion :: B.ByteString
                       } deriving (Ord, Eq)

instance Show Request where
    show request =  (B.unpack $ requestMethod request) ++ " "
                 ++ (B.unpack $ requestURI request) ++ " "
                 ++ (B.unpack $ requestVersion request)

-- | Read a HTTP request from a 'Handle'. For now, this will ignore the request
--   headers and body.
readRequest :: Handle -> Server Request
readRequest handle = do
    requestLine <- liftIO $ hGetLine handle
    let [method, uri, version] = map trim $ split " " requestLine
    return $ Request { requestMethod = B.pack method
                     , requestURI = B.pack uri
                     , requestVersion = B.pack version
                     }

-- | Simple representation of the HTTP response we send back.
data Response = Response { responseVersion :: B.ByteString
                         , responseStatusCode :: Int
                         , responsePhrase :: B.ByteString
                         , responseHeaders :: M.Map B.ByteString B.ByteString
                         , responseBody :: B.ByteString
                         } deriving (Ord, Eq)

instance Show Response where
    show response =  (B.unpack $ responseVersion response) ++ " "
                  ++ (show $ responseStatusCode response) ++ " "
                  ++ (B.unpack $ responsePhrase response)

-- | A default response.
defaultResponse :: Response
defaultResponse = Response { responseVersion = B.pack "HTTP/1.1"
                           , responseStatusCode = 0
                           , responsePhrase = B.empty
                           , responseHeaders = M.empty
                           , responseBody = B.empty
                           }

-- | Create a response for a given HTTP request.
createResponse :: Request -> Server Response
createResponse request | requestMethod request == B.pack "GET" = createGetResponse request
                       | otherwise = return $ createErrorResponse 501 (B.pack "Not Implemented")

-- | Create a simple error response.
createErrorResponse :: Int          -- ^ Error code.
                    -> B.ByteString -- ^ Error phrase.
                    -> Response     -- ^ Result.
createErrorResponse statusCode phrase = defaultResponse
    { responseStatusCode = statusCode
    , responsePhrase = phrase
    , responseHeaders = M.singleton (B.pack "Content-Type") (B.pack "text/html")
    , responseBody = B.pack $  "<html> <head> <title>" ++ show statusCode ++ "</title> </head>"
                            ++ "<body> <h1>" ++ show statusCode ++ "</h1>\n"
                            ++ "<p>" ++ (B.unpack phrase) ++ "</p> </body> </html>"
    }

-- | Create a simple get response.
createGetResponse :: Request -> Server Response
createGetResponse request = do
    -- Construct the complete fileName of the requested resource.
    config <- ask
    let uri = B.unpack (requestURI request)
    isDirectory <- liftIO $ doesDirectoryExist $ documentRoot config ++ uri
    let fileName = (documentRoot config) ++ if isDirectory then uri ++ "/index.html"
                                                           else uri

        create200 = do
            body <- B.readFile fileName
            let headers = [ (B.pack "Content-Length", B.pack $ show $ B.length body)
                          ] ++ getMIMEHeader fileName
            return $ defaultResponse { responseStatusCode = 200
                                     , responsePhrase = B.pack "OK"
                                     , responseHeaders = (responseHeaders defaultResponse) 
                                            `M.union` M.fromList headers
                                     , responseBody = body
                                     }

        -- Called when an error occurs during the creation of a 200 response.
        create500 e = do writeChan (logChannel config) $ "Internal Error: " ++ show e
                         return $ createErrorResponse 500 (B.pack "Internal Server Error")

    -- Send back the page if found.
    exists <- liftIO $ doesFileExist fileName
    if exists then do response <- liftIO $ catch create200 create500
                      return response
              else do liftIO $ writeChan (logChannel config) $ "Not Found: " ++ fileName
                      return $ createErrorResponse 404 (B.pack "Not Found")

-- | Get the mime header for a certain filename. This is based on the extension
--   of the given 'FilePath'.
getMIMEHeader :: FilePath -> [(B.ByteString, B.ByteString)]
getMIMEHeader fileName = case result of (Just x) -> [(B.pack "Content-Type", B.pack x)]
                                        Nothing  -> []
    where result = lookup (takeExtension fileName) [ (".css", "text/css")
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
    liftIO $ writeChan (logChannel config) $ show request ++ " => " ++ show response

    -- Send the response back to the handle.
    liftIO $ putResponse response
    where putResponse response = do B.hPutStr handle $ B.intercalate (B.pack " ")
                                          [ responseVersion response
                                          , B.pack $ show $ responseStatusCode response
                                          , responsePhrase response
                                          ]
                                    hPutStr handle "\r\n"
                                    mapM_ putHeader (M.toList $ responseHeaders response)
                                    hPutStr handle "\r\n"
                                    B.hPutStr handle $ responseBody response
                                    hPutStr handle "\r\n"
                                    hClose handle

          putHeader (key, value) = B.hPutStr handle $ key `B.append` B.pack ": "
                                                        `B.append` value `B.append` B.pack "\r\n"

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
                           return ()

    -- Handle logging in a separate thread
    forkIO (log logChan)

    writeChan logChan $ "Starting hakyll server on port " ++ show port ++ "..."
    socket <- listenOn (PortNumber port)
    forever (listen socket)
    where 

