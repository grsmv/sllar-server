module Main where

import qualified Server
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    if null args then argFailure
                 else case head args of
                        "server"   -> Server.start
                        "stop"     -> putStrLn "stop server"
                        "update"   -> putStrLn "update info"


-- Raising exit with message if number of arguments is incorrect
argFailure :: IO ()
argFailure = do
    putStrLn "Specify argument"
    exitFailure
