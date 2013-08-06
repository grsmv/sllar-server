module Main where

-- Sllar
import qualified Server
import Common
import Config
import Examination (checkFile)
import System.Posix.Process
import Paths_sllar_server

-- System
import Control.Monad
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import Text.Regex.Posix ((=~))


main :: IO ()
main = do
    args <- getArgs
    withArgs args $
      case head args of
        "start"    -> Server.start
        "stop"     -> Server.stop
        "update"   -> putStrLn "update info"
        "env"      -> env
        "path"     -> do path <- getDataFileName "resources/packages/"
                         putStrLn path
        _          -> argFailure


--
-- Raising exit with message if number of arguments is incorrect
--
argFailure :: IO ()
argFailure = failDown "No arguments specified"


--
-- Wrapper, that checks if any argument specified
-- Input: arguments, function to applicationy if arguments exists
-- Output: wrapped function
--
withArgs :: [String] -> IO () -> IO ()
withArgs args f = if null args
                    then failDown "No arguments specified"
                    else f


--
-- Getting all important information about current
-- Sllar-server installation.
-- Output: IO action
--
env :: IO ()
env = do
    let pidFile = "tmp/sllar-server.pid"
        (p, y) = (putStrLn, yellow) -- shortcuts
    sharePath <- getDataFileName ""
    Just config' <- config
    tmpFileExistence <- checkFile $ sharePath ++ pidFile

    (f, s) <- if tmpFileExistence
                then do pid <- readFile $ sharePath ++ pidFile
                        processExistence <- isProcessExists pid
                        return $ if processExistence
                                   then (green, "running (pid " ++ pid ++ ")")
                                   else (red, "stopped")
                else return (red, "stopped")

    -- formatting nice message
    p ""
    y "Current state:    "; f $ s ++ "\n"
    y "Port:             "; print $ port config'
    y "Packages folder:  "; p $ sharePath ++ "packages/"
    y "Config file:      "; p $ sharePath ++ "config"
    y "PID file:         "; p $ sharePath ++ pidFile
    p ""


--
-- Cheking if process running right now
-- Input: Process ID
-- Output: boolean state
--
isProcessExists :: String -> IO Bool
isProcessExists pid = do
    (_, stdout, _) <- readProcessWithExitCode "ps" ["-ewwo", "pid,args"] ""
    return $ case length $ filter (=~ ("^" ++ pid ++ " ")) (lines stdout) of
               0 -> False
               _ -> True
