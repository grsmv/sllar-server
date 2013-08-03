module Main where

import qualified Server
import Common
import System.Environment (getArgs)
import Paths_sllar_server

main :: IO ()
main = do
    args <- getArgs
    withArgs args $
      case head args of
        "setup"    -> putStrLn "setup your environment"
        "start"    -> Server.start
        "stop"     -> Server.stop
        "update"   -> putStrLn "update info"
        "path"     -> do path <- getDataFileName "resources/html/index.html"
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
