module Server ( start
              , stop ) where

-- Sllar
import Config
import Paths_sllar_server

-- System
import Control.Concurrent
import Control.Monad (forever)
import Network
import System.Directory (removeFile)
import System.IO
import System.Process (system)
import System.Posix.Process (getProcessID)
import System.Posix.Daemonize (daemonize)


data Request     = Request { rtype :: RequestType, path :: String }
data Response    = Response { body :: String, restype :: String }
data RequestType = GET | POST deriving Show

instance Show Request where
    show r = "Request {" ++ show (rtype r) ++ " " ++ path r ++ "}"

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

      -- getting data from server's config
      config' <- config
      let port' = case config' of
                    Just cfg -> port cfg
                    _        -> 5002

      sock <- listenOn $ PortNumber (fromInteger port')

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
    tmpFolder <- getDataFileName "tmp/"
    let tmpFilePath = tmpFolder ++ "sllar-server.pid"
    pid <- readFile tmpFilePath
    system $ "kill " ++ pid
    removeFile tmpFilePath
    putStrLn "sllar-server was stopped"


{------------------------------------------------------------------------------
                                Request handling
------------------------------------------------------------------------------}

--
-- Routing request to a specific content
-- Input: incoming request
-- Output: content for a specific route
-- TODO: `json` and `html` move to a separate functions
--
router :: Request -> IO Response
router request = do
    indexTemplatePath <- getDataFileName "html/index.html"
    index <- readFile indexTemplatePath
    let Request r p = request
        (h, j) = ("text/html", "application/json")
        (r', t) = case (r, p) of
                --  (get, "/")         -> html index
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
    tmpFolder <- getDataFileName "tmp/"
    let pidfile = tmpFolder ++ "sllar-server.pid"
        pidStr = show pid :: String
    writeFile pidfile pidStr
