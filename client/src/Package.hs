module Package
  ( install
  , publish
  , showInfo
  , initialize ) where

import System.Directory
import Examination (checkDirectory)
import Common


--
-- Creating template for a package
-- Input: package's name
--
initialize :: String -> IO ()
initialize name = do 
    currentDirectory <- getCurrentDirectory
    isCurrentDirectoryOK <- checkDirectory currentDirectory

    if isCurrentDirectoryOK
        then do let packageDir = currentDirectory ++ "/" ++ name
                createDirectory packageDir
                createDirectory $ packageDir ++ "/dist"

                writeFile (packageDir ++ "/" ++ name ++ ".sllar") $
                    "name:        " ++ name ++ "\n\
                    \description: A few words on library\n\
                    \author:      John Doe <john@doe.com>\n\
                    \maintainer:  Jane Doe\n\
                    \license:     Chosen license (MIT, BSD etc)\n\
                    \copyright:   (C) 2013 John & Jane Doe\n\
                    \version:     0.0.1"

                writeFile (packageDir ++ "/" ++ name ++ ".sl") $
                    "// Library name: " ++ name ++ "\n" ++
                    "// Author: John Doe"

                putStrLn $ "Folder and Sllar files for package \"" ++ name ++ "\" successfuly created"

        else failDown "Current directory is not writable"


--
-- Template for installing packages function
--
install :: [String] -> IO ()
install packages = putStrLn $ "Installing " ++ unwords packages


--
--
--
showInfo :: String -> IO ()
showInfo name = putStrLn $ "Show information about package " ++ name


--
--
--
publish :: [String] -> IO ()
publish packages = putStrLn $ "Publish package list: " ++ unwords packages