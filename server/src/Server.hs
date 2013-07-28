module Server (start) where

import Settings
import Control.Concurrent
import Control.Monad (forever)
import Network
import System.Directory (getCurrentDirectory)
import System.IO
import System.Posix.Process (getProcessID)


{------------------------------------------------------------------------------
                                    Server
------------------------------------------------------------------------------}

data RequestType = GET | POST deriving Show

data Request = Request { rtype :: RequestType
                       , path :: String }

data Response = Response { body :: String
                         , restype :: String }

instance Show Request where
    show r = "Request {" ++ show (rtype r) ++ " " ++ path r ++ "}"


-- | Starting a web-server, receiving and routing requests
start :: IO ()
start = withSocketsDo $ do
    let port = read (setting "port") :: Integer
    sock <- listenOn $ PortNumber (fromInteger port)
    forever $ do
      (handle, _, _) <- accept sock

      forkIO $ do
         request <- hGetContents handle
         hPutStr handle $ template (router (parseRequest request))
         hFlush handle
         hClose handle


-- | Routing request to a specific content
router :: Request -- ^ incoming request
       -> Response  -- ^ content for a specific route
router request =
    Response r' t
    where Request r p = request
          (h, j) = ("text/html", "application/json")
          (r', t) = case (r, p) of
                      (GET, "/")         -> ("root",                        h)
                      (GET, "/packages") -> ("[{'name':'A'},{'name':'B'}]", j)
                      _                  -> ("root",                        h)


-- | Wrapping content to a http request headers
template :: Response -- ^ data for response
         -> String   -- ^ final response
template Response { body = b, restype = t } =
    "HTTP/1.0 200 OK\r\n" ++
    "Content-type:" ++ t ++ ";charset=utf-8\r\n" ++
    "Content-Length: " ++ show (length b) ++ "\r\n\r\n" ++
    b ++ "\r\n"

{------------------------------------------------------------------------------
                                  Helpers
------------------------------------------------------------------------------}

-- | Parsing incoming request
parseRequest :: String  -- ^ raw string with request headers
             -> Request -- ^ parsed request details
parseRequest requestHeaders =
    case words (head (lines requestHeaders)) of
      [t, p, _] -> Request { rtype=fromString t, path=p }


-- | Converting string presentation of request type into specific type
fromString :: String      -- ^ string presentation of request type
           -> RequestType -- ^ value of a specific type
fromString t = case t of
                 "GET" -> GET
                 "POST" -> POST


-- | Memoizing Process PID, recording it to a pid file
writePid :: IO ()
writePid = do
    pid <- getProcessID
    pwd <- getCurrentDirectory
    let pidStr = show pid :: String
    writeFile (pwd ++ "/server.pid") pidStr
