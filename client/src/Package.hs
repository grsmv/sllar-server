module Package (install) where

install :: [String] -> IO ()
install pkgs = putStrLn $ "Installing " ++ unwords pkgs
