module Common
  ( failDown
  , red
  , green
  , yellow ) where

import System.Exit (exitFailure)
import System.Console.ANSI

--
-- Graceful dying with message
-- Input: message to show before death
--
failDown :: String -> IO ()
failDown s = do red "Error: "
                putStrLn s
                exitFailure


--
-- Generic color-printing helper
-- Input: text to colorize, Color to colorize in
-- Output: IO action
--
color :: String -> Color -> IO ()
color msg color' = do
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid color' ]
    putStr msg
    setSGR [Reset]


--
-- Bunch of public colorizing helpers
-- Input: text to colorize
-- Output: IO action
--
red, green, yellow :: String -> IO ()
red msg = color msg Red
green msg = color msg Green
yellow msg = color msg Yellow
