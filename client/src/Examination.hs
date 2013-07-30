module Examination ( checkDirectory
                   , checkFile
                   , checkEnv
                   ) where

import Common
import System.Directory
import System.Environment
import Data.List (find)


-- | Generic checker of precense of file or directory and permissions
genericCheck :: (String -> IO Bool) -- ^ checking function
             -> String              -- ^ path to check
             -> IO Bool
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


-- | Checking presence and permissions for specified directory
checkDirectory :: String  -- ^ path to directory
               -> IO Bool
checkDirectory = genericCheck doesDirectoryExist


-- | Checking presence and permissions for specified file
checkFile :: String   -- ^ path to file
          -> IO Bool
checkFile = genericCheck doesFileExist


-- | Return the value of the environment variable var, or Nothing if there is no such value.
-- NOTE: replace this by standard `lookupEnv` after moving to 7.6.x
lookupEnv' :: String             -- ^ variable name to search
           -> IO (Maybe String)
lookupEnv' k = do
  env <- getEnvironment
  let result = find (\(k', _) -> k' == k) env
  return $ case result of
    Just (_, v) -> Just v
    _           -> Nothing


-- | Checking presence of environment variable and presence of folder on
-- which this variable points.
checkEnv :: String  -- ^ variable name to search
         -> IO Bool
checkEnv envVar = do
  val <- lookupEnv' envVar
  case val of
    Just v  -> checkDirectory v
    Nothing -> do failDown (envVar ++ " is not defined")
                  return False
