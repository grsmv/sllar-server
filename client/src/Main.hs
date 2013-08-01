module Main where

import Examination (checkEnv)
import Common
import Control.Exception (onException)
import Control.Monad (when)
import System.Environment (getArgs)


main :: IO ()
main =
  do env <- checkEnv "GOPATH"
     when env $
       do args <- getArgs
          withArgs args $
            do let p = putStrLn
               case head args of
                 "setup"   -> p "setup your environment"
                 "list"    -> p "list of packages"
                 "update"  -> p "update list of packages from Sllar repository"
                 "publish" -> p "send package to a Sllar repository"
                 "help"    -> p "help"
                 "env"     -> p "show your current configuration"
                 "install" -> withName $ p ("install package " ++ args !! 1)
                 "show"    -> withName $ p ("show information about package " ++ args !! 1)
                 "create"  -> withName $ p ("create a skeleton of a package with name " ++ args !! 1)
                 _         -> p "help"

--
-- Wrapper around actions with package name.
-- Stops application execution if package name not given
-- Input: function to wrap
-- Output: funtion wrapped by package name existence verification
--
withName :: IO a -> IO a
withName action =
    onException action
      (failDown "Specify package name")


--
-- Wrapper, that checks if any argument specified
-- Input: arguments, function to applicationy if arguments exists
-- Output: wrapped function
--
withArgs :: [String] -> IO () -> IO ()
withArgs args f = if null args
                    then failDown "No arguments specified"
                    else f
