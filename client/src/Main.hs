module Main where

import Examination (checkEnv)
import Control.Exception (onException, finally)
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)


main :: IO ()
main =
    withArguments $
      do args <- getArgs
         let p = putStrLn
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


withArguments :: IO () -> IO ()
withArguments action =
    finally (do args <- getArgs
                when (null args) $ putStrLn "specify argument")
            (onException action exitFailure)


-- | Wrapper around actions with package name.
-- Stops application execution if package name not given
withName :: IO a -> IO a
withName action =
    onException action
      (do print "Specify package name"
          exitFailure)
