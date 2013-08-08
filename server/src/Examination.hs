module Examination
  ( checkDirectory
  , checkFile
  , checkEnv
  ) where

import Common
import System.Directory
import System.Environment
import Data.List (find)


--
-- Generic checker of precense of file or directory and permissions
-- Input: checking function
-- Output: path to check
genericCheck :: (String -> IO Bool) -> String -> IO Bool
genericCheck checker path = do
  presence <- checker path
  if presence
    then do
      permissions <- getPermissions path
      if writable permissions
        then return True
        else do failDown (path ++ " isn't writable")
                return False
    else do failDown (path ++ " not existing")
            return False


--
-- Checking presence and permissions for specified path (directory or file)
-- Input: path to directory
-- Output: boolean result
--
checkDirectory, checkFile :: String -> IO Bool
checkDirectory = genericCheck doesDirectoryExist
checkFile = genericCheck doesFileExist


--
-- Return the value of the environment variable var,
-- or Nothing if there is no such value.
-- NOTE: replace this by standard `lookupEnv` after moving to 7.6.x
-- Input: env variable name to search
-- Output: boolean result
--
lookupEnv' :: String -> IO (Maybe String)
lookupEnv' k = do
  env <- getEnvironment
  let result = find (\(k', _) -> k' == k) env
  return $ fmap snd result


--
-- Checking presence of environment variable and presence
-- of folder on which this variable points.
-- Input: variable name to search
-- Output: boolean result
--
checkEnv :: String -> IO Bool
checkEnv envVar = do
  val <- lookupEnv' envVar
  case val of
    Just v  -> checkDirectory v
    Nothing -> do failDown (envVar ++ " is not defined")
                  return False
