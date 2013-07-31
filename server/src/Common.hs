module Common (failDown) where

import System.Exit (exitFailure)

--
-- Graceful dying with message
-- Input: message to show before death
--
failDown :: String -> IO ()
failDown s = do putStrLn ("Error: " ++ s)
                exitFailure
