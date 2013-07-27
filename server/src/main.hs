module Main where

import Control.Concurrent
import Control.Monad (forever)
import Network
import System.Directory (getCurrentDirectory)
import System.IO
import System.Posix.Process (getProcessID)

data RequestType = GET | POST deriving Show
data Request = Request { rtype :: RequestType
                       , path :: String }

instance Show Request where
    show r = "Request {" ++ show (rtype r) ++ " " ++ path r ++ "\n}"

main :: IO ()
main = withSocketsDo $ do
         sock <- listenOn $ PortNumber 5002
         forever $ do
           (handle, _, _) <- accept sock
           parseRequest handle
           forkIO $ do
              hPutStr handle msg
              hFlush handle
              hClose handle


parseRequest :: Handle -> IO ()
parseRequest handle = do
    content <- hGetContents handle
    putStrLn content


msg :: String
msg = "HTTP/1.0 200 OK\r\nContent-Length: 6\r\n\r\nSllar!\r\n"


writePid :: IO ()
writePid = do
    pid <- getProcessID
    pwd <- getCurrentDirectory
    let pidStr = show pid :: String
    writeFile (pwd ++ "/server.pid") pidStr
