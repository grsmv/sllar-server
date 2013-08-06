module Package
  ( install
  , list
  , publish ) where

--
-- Template for installing packages function
--
install :: [String] -> IO ()
install packages = putStrLn $ "Installing " ++ unwords packages


--
--
--
show' :: IO ()
show' = putStrLn "Show information about certain package"


--
--
--
publish :: [String] -> IO ()
publish packages = putStrLn "Publish package list: " ++ unwords packages


--
--
--
create :: String -> IO ()
create = putStrLn "Creating new package from template"
