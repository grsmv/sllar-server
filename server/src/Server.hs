module Server (start) where

import Control.Concurrent
import Control.Monad (forever)
import Network
import System.Directory (getCurrentDirectory)
import System.IO
import System.Posix.Process (getProcessID)


{------------------------------------------------------------------------------
                                   Settings
------------------------------------------------------------------------------}

data Setting = Setting { k :: String, v :: String } deriving Show


settings :: [Setting]
settings = [ Setting "port" "5002"
           , Setting "tmp"  "/Users/sergey/Desktop/" ]


-- | Searching for a specific key through list of settings
settingValue :: String -- ^ key
             -> String -- ^ value
settingValue key = v . head $ filter (\s -> k s == key) settings

{------------------------------------------------------------------------------
                                    Server
------------------------------------------------------------------------------}


data RequestType = GET | POST deriving Show
data Request = Request { rtype :: RequestType , path :: String }

instance Show Request where
    show r = "Request {" ++ show (rtype r) ++ " " ++ path r ++ "}"


start :: IO ()
start = withSocketsDo $ do
    let port = read (settingValue "port") :: Integer
    sock <- listenOn $ PortNumber (fromInteger port)
    forever $ do
      (handle, _, _) <- accept sock

      -- getting request details
      request <- hGetContents handle
      print $ parseRequest request

      forkIO $ do
         hPutStr handle index
         hFlush handle
         hClose handle


{------------------------------------------------------------------------------
                               Server helpers
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


{------------------------------------------------------------------------------
                               Specific routes
------------------------------------------------------------------------------}

index :: String
index = "HTTP/1.0 200 OK\r\nContent-Length: 6\r\n\r\nSllar!\r\n"
