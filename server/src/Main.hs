{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import qualified Config
import Paths_sllar_server
import qualified Server
import qualified Templates

-- system
import Data.Version (showVersion)
import Data.Text.Template
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import Text.Regex.Posix ((=~))
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as E


main :: IO ()
main = do
    args <- getArgs
    withArgs args $
      case head args of
        "start"    -> Server.start
        "stop"     -> Server.stop
        "renew"    -> putStrLn "renew info"
        "info"     -> info
        "help"     -> help
        _          -> help


--
-- Getting all important information about current
-- Sllar-server installation.
-- Output: IO action
--
info :: IO ()
info = do
    sharedPath <- getDataFileName ""
    port <- Config.port . fromMaybe (Config.Config 5000) <$> Config.config
    state <- serverState

    let context assocs x = fromMaybe "" $ lookup x assocs
        ctx :: Context
        ctx = context [ ("state", T.pack state)
                      , ("port",  T.pack $ show port)
                      , ("version", T.pack $ showVersion version)
                      , ("numberOfPackages", T.pack "42")
                      , ("sharedPath", T.pack sharedPath)
                      ]

    S.putStrLn $ E.encodeUtf8 $ substitute (T.pack Templates.info) ctx


--
-- Showing helping information
-- Output: IO action
--
help :: IO ()
help =
    S.putStrLn $ E.encodeUtf8 $ substitute (T.pack Templates.help) ctx
      where
        context assocs x = fromMaybe "" $ lookup x assocs
        ctx = context [] :: Context


--
-- Cheking if process running right now
-- Input: Process ID
-- Output: boolean state
--
isProcessExists :: String -> IO Bool
isProcessExists pid = do
    (_, stdout, _) <- readProcessWithExitCode "ps" ["-ewwo", "pid,args"] ""
    return $ not . null $ filter (=~ ("^" ++ pid ++ " ")) (lines stdout)


--
-- Checking server running state and returning
-- human-readable answer
-- todo: check why state is always "stopped"
--
serverState :: IO String
serverState = do
    let pidFile = "tmp/sllar-server.pid"
    sharedPath <- getDataFileName ""
    doesPidFileExists <- doesFileExist $ sharedPath ++ pidFile
    if doesPidFileExists
       then do processExistence <- readFile (sharedPath ++ pidFile) >>= isProcessExists
               return $ if processExistence then "running" else "stopped"
       else return "stopped"


--
-- Wrapper, that checks if any argument specified
-- Input: arguments, function to applicationy if arguments exists
-- Output: wrapped function
--
withArgs :: [String] -> IO () -> IO ()
withArgs args f = if null args then help else f
