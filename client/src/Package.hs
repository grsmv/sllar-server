{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Package
  ( install
  , publish
  , showInfo
  , initialize
  , pack ) where

-- Sllar
import Common
import qualified Config
import Examination (checkDirectory)

-- System
import Codec.Archive.Tar (create)
import Control.Exception
import Control.Monad (unless, when)
import Data.Char
import Data.List (isInfixOf, (\\), elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Yaml
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.Directory
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64 as Base64 (encode)

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
config path' = do rawText <- readFile path'
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
    when doesTargetFolderExists $
      failDown $ "Folder with name \"" ++ p ++ "\" already exists"

    if isCurrentDirectoryOK
        then do let packageDir = currentDirectory ++ "/" ++ p
                mapM_ createDirectory [packageDir, packageDir ++ "/dist"]

                writeFile (packageDir ++ "/" ++ p ++ ".sllar") $
                    "name:        " ++ p ++ "\n" ++
                    "description: A few words on library\n" ++
                    "author:      John Doe <john@doe.com>\n" ++
                    "maintainer:  Jane Doe\n" ++
                    "license:     Chosen license (MIT, BSD etc)\n" ++
                    "copyright:   (C) 2013 John & Jane Doe\n" ++
                    "version:     0.0.1"

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
    files <- getCurrentDirectory >>= getDirectoryContents

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
-- Sending compressed package to server
--
publish :: IO ()
publish = do
    (tarDist, tarFiles) <- tarFilesInDist

    -- getting repositories from sllar-server config
    cfg <- Config.config
    let repositories = Config.repositories $ fromMaybe (Config.Config []) cfg

    -- checking if any repositories available
    if not . null $ repositories
      then do
              -- if more than one repo available in config - ask to select one
              repo <- if length repositories > 1
                        then do putStrLn "Please choose between available repositories:"
                                mapM_ (\r -> putStrLn $ "  [" ++ show (fromMaybe 0 (elemIndex r repositories) + 1) ++ "] " ++ r) repositories
                                n <- getRepoNumber repositories
                                return $ repositories !! (n - 1)
                        else return $ head repositories

              -- sending maximum available version of package to repository
              sendPackage repo $ tarDist ++ "/" ++ maximum tarFiles
      else failDown "There's no repositories in config"


--
-- Getting list of already packed tarballs, if none - pack one
-- and return refreshed list
-- Output: tarballs folder, list of tarballs
--
tarFilesInDist :: IO (String, [String])
tarFilesInDist = do
    dist <- fmap (++ "/dist") getCurrentDirectory
    ifDistExists <- doesDirectoryExist dist
    filesinDist <- getDirectoryContents dist
    let tarFiles = filter (".sllar.tar" `isInfixOf`) filesinDist

    -- if there's no `dist` or tar files inside `dist` files - pack
    if not ifDistExists || null tarFiles
       then do pack
               tarFilesInDist
       else return (dist, tarFiles)


--
-- Asking user to pick one from available repositories
-- Input: list of repositories
-- Output: index of chosen one
--
getRepoNumber :: [String] -> IO Int
getRepoNumber repos' = do
    d <- getLine
    let num = digitToInt $ head d
    if isDigit (head d) && num <= length repos' && num > 0
      then return $ digitToInt (head d)
      else do putStrLn $ "Please retry [1..." ++ show (length repos') ++ "]"
              getRepoNumber repos'


--
-- Sending selected package to repository, reading response
-- Input: repository url, path to tarball
--
sendPackage :: String -> FilePath -> IO ()
sendPackage repoUrl packagePath = do
    req0 <- parseUrl $ repoUrl ++ "/publish"
    fileContents <- BS.readFile packagePath
    let packagePathSplitted = splitOn "/" packagePath

        -- prepearing request
        request = req0 { method = methodPost
                       , requestHeaders =
                           [ ("Content-Type", "application/x-tar")
                           , ("tarName", BS.pack $ last packagePathSplitted)
                           , ("tarBody", Base64.encode fileContents) ]}

    res <- withManager $ httpLbs request
    let Status code status = responseStatus res
        body' = BL.unpack $ responseBody res
        (p, f) = (putStrLn, failDown) -- shortcuts

    if code == 200 && status == "OK"
       then case body' of
              "ok" ->               p "Sllar package successfully published"
              "version_exists" ->   f "Same version already exists. Increase it."
              "incorrect_config" -> f "Incorrect <package_name>.sllar file"
              "no_config" ->        f "No <package_name>.sllar file"
              _ ->                  f "Tack, ni bröt internet"

       -- todo: show failure reason
       else failDown "Package publishing failed"

    -- onException (do res <- withManager $ httpLbs request
    --                 let Status code status = responseStatus res
    --                     body' = BL.unpack $ responseBody res
    --                     (p, f) = (putStrLn, failDown) -- shortcuts

    --                 if code == 200 && status == "OK"
    --                    then case body' of
    --                           "ok" ->               p "Sllar package successfully published"
    --                           "version_exists" ->   f "Same version already exists. Increase it."
    --                           "incorrect_config" -> f "Incorrect <package_name>.sllar file"
    --                           "no_config" ->        f "No <package_name>.sllar file"
    --                           _ ->                  f "Tack, ni bröt internet"

    --                    -- todo: show failure reason
    --                    else failDown "Package publishing failed")
    --             (failDown $ "Repository " ++ repoUrl ++ " isn't available")


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
