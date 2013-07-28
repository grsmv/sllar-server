module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)


main :: IO ()
main = do
    args <- getArgs
    if null args then argFailure
                 else case head args of
                        "list"    -> p "list of packages"
                        "update"  -> p "update list of packages from Sllar repository"
                        "publish" -> p "send package to a Sllar repository"
                        "help"    -> p "help"
                        "env"     -> p "show your current configuration"
                        "install" -> do package <- packageName args
                                        p $ "install package " ++ package
                        "show"    -> do package <- packageName args
                                        p $ "show information about package " ++ package
                        "create"  -> do package <- packageName args
                                        p $ "create a skeleton of a package with name " ++ package
                        _ -> argFailure


-- Getting package name from list of arguments
packageName :: [String] -- List of arguments
            -> IO String
packageName args = if length args > 1 then return (args !! 1)
                                      else do p "Specify package name"
                                              exitFailure

-- Raising exit with message if number of arguments is incorrect
argFailure :: IO ()
argFailure = do
    p "Specify argument"
    exitFailure


-- Shortcut for `putStrLn`
p :: String -> IO ()
p = putStrLn
