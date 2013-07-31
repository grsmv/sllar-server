module Main where

import qualified Server
import Common
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    if null args then argFailure
                 else case head args of
                        "setup"    -> putStrLn "setup your environment"
                        "start"    -> Server.start
                        "stop"     -> Server.stop
                        "update"   -> putStrLn "update info"
                        _          -> argFailure


--
-- Raising exit with message if number of arguments is incorrect
--
argFailure :: IO ()
argFailure = failDown "No arguments specified"
