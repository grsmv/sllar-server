module Package
  ( install
  , publish
  , show'
  , create ) where

--
-- Template for installing packages function
--
install :: [String] -> IO ()
install packages = putStrLn $ "Installing " ++ unwords packages


--
--
--
show' :: String -> IO ()
show' name = putStrLn $ "Show information about package " ++ name


--
--
--
publish :: [String] -> IO ()
publish packages = putStrLn $ "Publish package list: " ++ unwords packages


--
--
--
create :: String -> IO ()
create name = putStrLn $ "Creating new package from template with name " ++ name
