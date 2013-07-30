module Main where

import Examination (checkEnv)
import Common
import Control.Exception (onException)
import Control.Monad (when)
import System.Environment (getArgs)


main :: IO ()
main =
  do env <- checkEnv "TMT_HOME"
     when env $
       do args <- getArgs
          if null args
            then failDown "Specify an argument(s)"
            else do let p = putStrLn
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

-- | Wrapper around actions with package name.
-- Stops application execution if package name not given
withName :: IO a -> IO a
withName action =
    onException action
      (failDown "Specify package name")
