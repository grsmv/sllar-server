module Common (failDown) where

import System.Exit (exitFailure)

failDown :: String -> IO ()
failDown s = do putStrLn ("Error: " ++ s)
                exitFailure
