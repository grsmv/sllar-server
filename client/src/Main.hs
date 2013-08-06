module Main where

-- Sllar
import Examination (checkEnv)
import Common
import qualified Package

-- System
import Control.Exception (onException)
import Control.Monad (when)
import System.Environment (getArgs)


main :: IO ()
main =
  do env <- checkEnv "TMPDIR"
     when env $
       do args <- getArgs
          withArgs args $
            do let p = putStrLn
               case head args of
                 "list"    -> p "list of packages"
                 "update"  -> p "update list of packages from Sllar repository"
                 "publish" -> p "send package to a Sllar repository"
                 "env"     -> p "show your current configuration"
                 "help"    -> p "help"
                 "install" -> withName $ Package.install (drop 1 args)
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
