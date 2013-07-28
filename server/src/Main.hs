module Main where

import qualified Server
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    if null args then argFailure
                 else case head args of
                        "start"    -> Server.start
                        "stop"     -> Server.stop
                        "update"   -> putStrLn "update info"
                        _          -> argFailure


-- Raising exit with message if number of arguments is incorrect
argFailure :: IO ()
argFailure = do
    putStrLn "Specify argument"
    exitFailure
