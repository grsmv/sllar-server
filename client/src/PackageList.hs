module PackageList 
  ( update
  , show' ) where

--
--
--
show' :: IO ()
show' = putStrLn "Displaying a list of available packages"


--
--
--
update :: IO ()
update = putStrLn "Updating package list from all available repositories"
