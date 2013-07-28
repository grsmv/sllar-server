module Main where

import Control.Exception (onException)
import System.Environment (getArgs)
import System.Exit (exitFailure)


main :: IO ()
main = do
    args <- getArgs
    if null args
      then do
        p "Specify argument"
        exitFailure
      else case head args of
             "list"    -> p "list of packages"
             "update"  -> p "update list of packages from Sllar repository"
             "publish" -> p "send package to a Sllar repository"
             "help"    -> p "help"
             "env"     -> p "show your current configuration"
             "install" -> withName $ p ("install package " ++ args !! 1)
             "show"    -> withName $ p ("show information about package " ++ args !! 1)
             "create"  -> withName $ p ("create a skeleton of a package with name " ++ args !! 1)
             _         -> p "help"
    where p = putStrLn


-- | Wrapper around actions with package name.
-- Stops application execution if package name not given
withName :: IO a -> IO a
withName action =
    onException action
      (do print "Specify package name"
          exitFailure)
