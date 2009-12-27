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
                       , requestHeaders :: M.Map B.ByteString B.ByteString
                       , requestBody :: B.ByteString
                       } deriving (Show, Ord, Eq)

readRequest :: Handle -> IO Request
readRequest handle = do
    requestLine <- hGetLine handle
    let [method, uri, version] = map trim $ split " " requestLine
    return $ Request { requestMethod = B.pack method
                     , requestURI = B.pack uri
                     , requestVersion = B.pack version
                     , requestHeaders = M.empty -- Ignore all headers for now.
                     , requestBody = B.empty -- Ignore request body for now.
                     }

data Response = Response { responseVersion :: B.ByteString
                         , responseStatusCode :: Int
                         , responsePhrase :: B.ByteString
                         , responseHeaders :: M.Map B.ByteString B.ByteString
                         , responseBody :: B.ByteString
                         } deriving (Show, Ord, Eq)

defaultResponse :: Response
defaultResponse = Response { responseVersion = B.pack "HTTP/1.1"
                           , responseStatusCode = 0
                           , responsePhrase = B.empty
                           , responseHeaders = M.empty
                           , responseBody = B.empty
                           }

createResponse :: Request -> IO Response
createResponse request | requestMethod request == B.pack "GET" = createGetResponse request
                       | otherwise = return $ defaultResponse { responseStatusCode = 501
                                                              , responsePhrase = B.pack "Not Implemented"
                                                              }

createGetResponse :: Request -> IO Response
createGetResponse request = do
    let uri = B.unpack (requestURI request)
    let fileName = "_site" ++ if uri == "/" then "/index.html"
                                            else B.unpack (requestURI request)
    exists <- doesFileExist fileName
    if exists then createGet fileName
              else create404
        where create404 = return $ defaultResponse { responseStatusCode = 404
                                                   , responsePhrase = B.pack "Not Found"
                                                   }

              createGet fileName = do
                    body <- B.readFile fileName
                    let headers = [ (B.pack "Content-Length", B.pack $ show $ B.length body)
                                  ] ++ getMIMEHeader fileName
                    return $ defaultResponse { responseStatusCode = 200
                                             , responsePhrase = B.pack "OK"
                                             , responseHeaders = (responseHeaders defaultResponse) 
                                                    `M.union` M.fromList headers
                                             , responseBody = body
                                             }

getMIMEHeader :: FilePath -> [(B.ByteString, B.ByteString)]
getMIMEHeader fileName = case result of (Just x) -> [(B.pack "Content-Type", B.pack x)]
                                        _ -> []
    where result = lookup (takeExtension fileName) [ (".css", "text/css")
                                                   , (".gif", "image/gif")
                                                   , (".htm", "text/html")
                                                   , (".html", "text/html")
                                                   , (".jpeg", "image/jpeg")
                                                   , (".jpg", "image/jpeg")
                                                   , (".js", "text/javascript")
                                                   , (".png", "image/png")
                                                   , (".txt", "text/plain")
                                                   , (".xml", "text/xml")  
                                                   ]

respond :: Handle -> IO ()
respond handle = do
    request <- readRequest handle
    response <- createResponse request
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
