module Examination ( checkDirectory
                   , checkFile
                   , checkEnv
                   ) where

  import System.Directory
  import System.Environment
  import System.Exit (exitFailure)
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
          else do
            putStrLn $ "Error: " ++ path ++ " isn't writable"
            exitFailure
      else do
        putStrLn $ "Error: " ++ path ++ " not existing"
        exitFailure


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
      Nothing -> do putStrLn $ "Error: $" ++ envVar ++ " is not defined"
                    exitFailure
