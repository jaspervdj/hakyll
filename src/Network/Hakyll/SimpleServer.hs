module Network.Hakyll.SimpleServer
    ( simpleServer
    ) where

import Network
import Control.Monad (forever, mapM_)
import System.IO (Handle, hClose, hGetLine, hPutStr)
import System.Directory (doesFileExist)
import Control.Concurrent (forkIO)
import System.FilePath (takeExtension)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Text.Hakyll.Util

data Request = Request { requestMethod :: B.ByteString
                       , requestURI :: B.ByteString
                       , requestVersion :: B.ByteString
                       } deriving (Ord, Eq)

instance Show Request where
    show request =  (B.unpack $ requestMethod request) ++ " "
                 ++ (B.unpack $ requestURI request) ++ " "
                 ++ (B.unpack $ requestVersion request)

readRequest :: Handle -> IO Request
readRequest handle = do
    requestLine <- hGetLine handle
    let [method, uri, version] = map trim $ split " " requestLine
    return $ Request { requestMethod = B.pack method
                     , requestURI = B.pack uri
                     , requestVersion = B.pack version
                     }

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

defaultResponse :: Response
defaultResponse = Response { responseVersion = B.pack "HTTP/1.1"
                           , responseStatusCode = 0
                           , responsePhrase = B.empty
                           , responseHeaders = M.empty
                           , responseBody = B.empty
                           }

createResponse :: Request -> IO Response
createResponse request | requestMethod request == B.pack "GET" = createGetResponse request
                       | otherwise = return $ errorResponse 501 (B.pack "Not Implemented")

errorResponse :: Int -> B.ByteString -> Response
errorResponse statusCode phrase = defaultResponse
    { responseStatusCode = statusCode
    , responsePhrase = phrase
    , responseHeaders = M.singleton (B.pack "Content-Type") (B.pack "text/html")
    , responseBody = B.pack $  "<html> <head> <title>" ++ show statusCode ++ "</title> </head>"
                            ++ "<body> <h1>" ++ show statusCode ++ "</h1>\n"
                            ++ "<p>" ++ (B.unpack phrase) ++ "</p> </body> </html>"
    }

createGetResponse :: Request -> IO Response
createGetResponse request = do
    let uri = B.unpack (requestURI request)
    let fileName = "_site" ++ if uri == "/" then "/index.html"
                                            else B.unpack (requestURI request)
    exists <- doesFileExist fileName
    if exists then catch (create200 fileName) create500
              else return $ errorResponse 400 (B.pack "Not Found")
        where create200 fileName = do
                    body <- B.readFile fileName
                    let headers = [ (B.pack "Content-Length", B.pack $ show $ B.length body)
                                  ] ++ getMIMEHeader fileName
                    return $ defaultResponse { responseStatusCode = 200
                                             , responsePhrase = B.pack "OK"
                                             , responseHeaders = (responseHeaders defaultResponse) 
                                                    `M.union` M.fromList headers
                                             , responseBody = body
                                             }

              create500 e = do putStrLn $ "Internal Error: " ++ show e
                               return $ errorResponse 500 (B.pack "Internal Server Error")

getMIMEHeader :: FilePath -> [(B.ByteString, B.ByteString)]
getMIMEHeader fileName = case result of (Just x) -> [(B.pack "Content-Type", B.pack x)]
                                        _ -> []
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

respond :: Handle -> IO ()
respond handle = do
    request <- readRequest handle
    response <- createResponse request
    putStrLn $ show request ++ " => " ++ show response
    B.hPutStr handle $ B.intercalate (B.pack " ") [ responseVersion response
                                                  , B.pack $ show $ responseStatusCode response
                                                  , responsePhrase response
                                                  ]
    hPutStr handle "\r\n"
    mapM_ putHeader (M.toList $ responseHeaders response)
    hPutStr handle "\r\n"
    B.hPutStr handle $ responseBody response
    hPutStr handle "\r\n"
    hClose handle
    where putHeader (key, value) = B.hPutStr handle $ key `B.append` B.pack ": "
                                                        `B.append` value `B.append` B.pack "\r\n"

simpleServer :: PortNumber -> IO ()
simpleServer port = do
    putStrLn $ "Starting hakyll server on port " ++ show port ++ "..."
    socket <- listenOn (PortNumber port)
    forever (listen socket)
    where listen socket = do (handle, _, _) <- accept socket
                             forkIO (respond handle)
                             return ()
