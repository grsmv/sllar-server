module Sllar.Common
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
-- Input: Color to colorize in, text to colorize
-- Output: IO action
--
color :: Color -> String -> IO ()
color color' msg = do
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
red = color Red
green = color Green
yellow = color Yellow
