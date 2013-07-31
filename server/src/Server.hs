module Server ( start
              , stop ) where

import Control.Concurrent
import Control.Monad (forever)
import Network
import Settings
import System.IO
import System.Process (system)
import System.Posix.Process (getProcessID)
import System.Posix.Daemonize (daemonize)


data Request     = Request { rtype :: RequestType , path :: String }
data Response    = Response { body :: String , restype :: String }
data RequestType = GET | POST deriving Show

instance Show Request where
    show r = "Request {" ++ show (rtype r) ++ " " ++ path r ++ "}"

progName :: String
progName = "sllar-server"

{------------------------------------------------------------------------------
                                Public API
------------------------------------------------------------------------------}

--
-- Starting a web-server, receiving and routing requests
--
start :: IO ()
start =
  daemonize $
    withSocketsDo $ do
      let port = read (setting "port") :: Integer
      sock <- listenOn $ PortNumber (fromInteger port)

      forkIO writePid

      forever $ do
        (handle, _, _) <- accept sock

        forkIO $ do
           request <- hGetContents handle
           response <- router (parseRequest request)
           hPutStr handle $ template response
           hFlush handle
           hClose handle


--
-- Killing sllar-server process by PID
--
stop :: IO ()
stop = do
    pid <- readFile $ setting "tmp" ++ "/" ++ progName ++ ".pid"
    system ("kill " ++ pid)
    putStrLn $ progName ++ " was stopped"


{------------------------------------------------------------------------------
                                Request handling
------------------------------------------------------------------------------}

--
-- Routing request to a specific content
-- Input: incoming request
-- Output: content for a specific route
--
router :: Request -> IO Response
router request = do
    index <- readFile $ setting "home" ++ "/resources/html/index.html"
    let Request r p = request
        (h, j) = ("text/html", "application/json")
        (r', t) = case (r, p) of
                    (GET, "/")         -> (index,                         h)
                    (GET, "/packages") -> ("[{'name':'A'},{'name':'B'}]", j)
                    _                  -> (index,                         h)
    return (Response r' t)


--
-- Wrapping content to a http request headers
-- Input: data for response
-- Output: final response
--
template :: Response -> String
template Response { body = b, restype = t } =
    "HTTP/1.0 200 OK\r\n" ++
    "Content-type:" ++ t ++ ";charset=utf-8\r\n" ++
    "Content-Length: " ++ show (length b) ++ "\r\n\r\n" ++
    b ++ "\r\n"


--
-- Parsing incoming request
-- Input: raw string with request headers
-- Output: parsed request details
--
parseRequest :: String -> Request
parseRequest requestHeaders =
    case words (head (lines requestHeaders)) of
      [t, p, _] -> Request { rtype=fromString t, path=p }


--
-- Converting string presentation of request type into specific type
-- Input: string presentation of request type
-- Output: value of a specific type
--
fromString :: String -> RequestType
fromString t = case t of
                 "GET" -> GET
                 "POST" -> POST


{------------------------------------------------------------------------------
                                  Helpers
------------------------------------------------------------------------------}

--
-- Memoizing Process PID, recording it to a pid file
--
writePid :: IO ()
writePid = do
    pid <- getProcessID
    let pidfile = setting "tmp" ++ "/" ++ progName ++ ".pid"
        pidStr = show pid :: String
    writeFile pidfile pidStr
