{-# LANGUAGE DeriveGeneric #-}

module Package
  ( install
  , publish
  , showInfo
  , initialize
  , pack ) where

-- Sllar
import Examination (checkDirectory)
import Common

-- System
import Control.Monad (unless)
import Codec.Archive.Tar (create)
import System.Directory
import Data.List (isInfixOf, (\\))
import Data.Yaml
import GHC.Generics
import qualified Data.ByteString.Char8 as BS

data Package = Package { name :: String, version :: String } deriving (Show, Generic)
instance FromJSON Package


--
--  *.sllar file is just a simple YAML-formatted datafile, so, in
--  case of data correctness, it's content can be represented as
--  instance of Package class
--  This function grabs a *.sllar file and tries to present it Input
--  system-recognizable form.
--
config :: FilePath -> IO (Maybe Package)
config path = do rawText <- readFile path
                 let packageConfig = decode (BS.pack rawText) :: Maybe Package
                 return packageConfig


--
-- Creating template for a package
-- Input: package's name
--
initialize :: String -> IO ()
initialize p = do
    currentDirectory <- getCurrentDirectory
    isCurrentDirectoryOK <- checkDirectory currentDirectory

    -- checking if there's no folder with same name
    doesTargetFolderExists <- doesDirectoryExist $ currentDirectory ++ "/" ++ p
    unless doesTargetFolderExists $
      failDown $ "Folder with name \"" ++ p ++ "\" already exists"

    if isCurrentDirectoryOK
        then do let packageDir = currentDirectory ++ "/" ++ p
                createDirectory packageDir
                createDirectory $ packageDir ++ "/dist"

                writeFile (packageDir ++ "/" ++ p ++ ".sllar") $
                    "name:        " ++ p ++ "\n\
                    \description: A few words on library\n\
                    \author:      John Doe <john@doe.com>\n\
                    \maintainer:  Jane Doe\n\
                    \license:     Chosen license (MIT, BSD etc)\n\
                    \copyright:   (C) 2013 John & Jane Doe\n\
                    \version:     0.0.1"

                writeFile (packageDir ++ "/" ++ p ++ ".sl") $
                    "// Library name: " ++ p ++ "\n" ++
                    "// Author: John Doe"

                putStrLn $ "Folder and Sllar files for package \"" ++ p ++ "\" successfuly created"
        else failDown "Current directory is not writable"


--
-- Packing current package into a tarball and storing it in
-- `dist` subfolder of package's directory
--
pack :: IO ()
pack = do
    currentDirectory <- getCurrentDirectory
    files <- getDirectoryContents currentDirectory

    -- checking *.sllar file existence
    let packageConfigList = filter (".sllar" `isInfixOf`) files
    if not . null $ packageConfigList
        then do let packageConfigFile = head packageConfigList
                packageConfig <- config packageConfigFile

                -- checking correctness of contents of *.sllar file
                case packageConfig of
                    Just cfg -> do let tmp = "dist"
                                   -- creating `dist` if it's not exists
                                   doesDistExists <- doesDirectoryExist tmp
                                   unless doesDistExists $ createDirectory tmp

                                   -- storing package ina a tarball
                                   let tarName = tmp ++ "/" ++ name cfg ++ "-" ++ version cfg ++ ".sllar.tar"
                                       junk = [tmp, ".", "..", ".DS_Store"]
                                   create tarName "" $ files \\ junk

                                   -- finish message
                                   putStrLn $ tarName ++ " created"
                    Nothing -> failDown $ "Verify correctness of " ++ packageConfigFile
        else failDown "Please create <PACKAGE_NAME>.sllar file"


--
--
--
publish :: IO ()
publish =
-- check if *.sllar file exists
-- check repos availability
-- if number of repos > 1 - ask to select one
-- if there's no dist/*.sllar.tar files - pack
-- send to a selected repo
-- read an answer - if all OK - renew repo's information
-- if fail - show error message
  putStrLn "publish"


--
-- Template for installing packages function
--
install :: [String] -> IO ()
install packages = putStrLn $ "Installing " ++ unwords packages


--
--
--
showInfo :: String -> IO ()
showInfo p = putStrLn $ "Show information about package " ++ p
