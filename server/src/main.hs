module Main where

import Control.Concurrent
import Network
import System.Directory (getCurrentDirectory)
import System.IO
import System.Posix.Daemonize (daemonize)
import System.Posix.Process (getProcessID)

main :: IO ()
main = do
    -- daemonize $ -- do
      pid <- getProcessID
      pwd <- getCurrentDirectory
      let pidStr = show pid :: String
      writeFile (pwd ++ "/server.pid") pidStr
      --
      withSocketsDo $ do
        sock <- listenOn $ PortNumber 5002
        loop sock

loop :: Socket -> IO ()
loop sock = do
   (h, _, _) <- accept sock
   forkIO $ body h
   loop sock
  where
   body h = do
      hPutStr h msg
      hFlush h
      hClose h

msg :: String
msg = "HTTP/1.0 200 OK\r\nContent-Length: 6\r\n\r\nSllar!\r\n"
