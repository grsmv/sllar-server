module Server ( start
              , stop ) where

-- Sllar
import Config
import Package (publish)
import Paths_sllar_server

-- System
import Control.Concurrent
import Control.Monad (forever)
import Data.Maybe
import Network
import System.Directory (removeFile)
import System.IO
import System.Process (system)
import System.Posix.Process (getProcessID)
import System.Posix.Daemonize (daemonize)

data Request = Request
             { rtype :: RequestType
             , path :: String
             , options :: [(String, String)]
             } deriving (Show)

data Response = Response
              { body :: String
              , restype :: String
              }

data RequestType = GET | POST deriving (Show, Read)

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
      let port' = port $ fromMaybe (Config 5000) config'
      sock <- listenOn $ PortNumber (fromInteger port')

      _ <- forkIO writePid

      forever $ do
        (handle, _, _) <- accept sock

        forkIO $ do
           request <- fmap (parseRequest . lines) (hGetContents handle)
           response <- router request
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
    _ <- system $ "kill " ++ pid
    removeFile tmpFilePath
    putStrLn "sllar-server was stopped"


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
    let Request rtype' path' options' = request
        (html, json, text) = ("text/html", "application/json", "text/plain")
        (bodyIO, respType) = case (rtype', path') of
                                  (GET,    "/packages") -> (return "[{'name':'A'}]", json)
                                  (POST,   "/publish")  -> (publish options',    text)

                                  -- everything else, including index
                                  _ -> (getDataFileName "html/index.html" >>= readFile, html)
    body' <- bodyIO
    return (Response body' respType)


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
parseRequest :: [String] -> Request
parseRequest lns =
    case words (head lns) of
      [t, p, _] -> Request { rtype=read t, path=p, options=parseRequestHelper(tail lns, []) }


--
-- Getting request headers (options)
--
parseRequestHelper :: ([String], [(String, String)]) -> [(String, String)]
parseRequestHelper ([], accum) = accum
parseRequestHelper (l:rest, accum)
    | length (words l) < 2 = accum
    | otherwise = parseRequestHelper(rest, accum ++ [(init . head . words $ l, unwords . tail . words $ l)] )


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
        pidStr = show pid
    writeFile pidfile pidStr
