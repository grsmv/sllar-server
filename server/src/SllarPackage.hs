{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- This module provides support for reading YAML-based contents
-- of *.sllar files, that coming with all Sllar packages.
-- It's used during reading and verification of incoming package.

module Package where

-- Sllar
import qualified Paths_sllar_server as Paths

-- System
import Codec.Archive.Tar (extract)
import Control.Monad
import Data.Data
import Data.List
import Data.Maybe (fromMaybe)
import Data.Yaml
import GHC.Generics
import System.Directory
import System.IO (hPutStr, hClose)
import System.IO.Temp (withTempDirectory, withTempFile)
import System.Path.NameManip (unslice_path)
import qualified Data.ByteString.Base64 as Base64 (decodeLenient)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.Utils as List
import qualified Data.String.Utils as String

-- Data type for presenting package essence, that comes from
-- user, that's why it differs from Package type, which is declared
-- in Package module. Main difference is in `version` field, instead
-- of `versions` in Package module. The difference exists because
-- package comes with only one (current) version from user.
data Package = Package
             -- mandatory fields
             { name
             , description
             , author
             , version :: String

             -- optional fields
             , maintainer
             , license
             , copyright
             , homepage
             , tracker :: Maybe String
             } deriving (Data, Typeable, Show, Generic)

instance FromJSON Package


--
-- Presenting Sllar package information in system-readable
-- format
-- Input: ByteString with *.sllar file contents
-- Output: instance of type Package or nothing
--
packageInfo :: BS.ByteString -> IO (Maybe Package)
packageInfo s = return (decode (BS.pack (correct (BS.unpack s))) :: Maybe Package)


--
-- Getting type's fields
-- Output: type's fields as list of strings
--
defaultFields :: [String]
defaultFields = List.replace ["versions"] ["version"] fields
    where fields = constrFields . toConstr $ package

          -- TODO: change this crap
          package = Package "" "" "" "" j j j j j
          j = Just ""


--
-- Analyzing raw data from YAML file and detecting fields,
-- that not present in YAML file (why?)
-- Input: data, all fields thar should be presented
-- Output: fields, that not presented in input data
--
unusedFileds :: String -> [String] -> [String]
unusedFileds str =
    filter (\e -> not $ (e ++ ":") `isInfixOf` String.replace " " "" str)


--
-- Corecting data in YAML file (adding fields that not presented,
-- but which should be presented).
-- Input: raw data from YAML files
-- Output: corrected data
--
correct :: String -> String
correct text = foldl (\acc e -> acc ++ e ++ ":\n") text fields
               where fields = unusedFileds text defaultFields


--
-- Recieving a package, testing it's correctness and storing it
-- Input: list of options as tuples
-- Output: action result
-- todo: move most of definitions of paths to a separate function
--
publish :: [(String, String)] -> IO String
publish options = do
    let value k = fromMaybe "" $ lookup k options
        tarName = value "tarName"
        tarBody = Base64.decodeLenient $ BS.pack (value "tarBody")
        ext = ".sllar.tar"
        tmpName = String.replace ext "" tarName

    tmp <- Paths.getDataFileName "tmp"
    packages <- Paths.getDataFileName "packages"

    -- unpacking and examining package
    withTempDirectory tmp tmpName $ \tmpDir ->         -- <tmp>/<package_name>.<rand>
      withTempFile tmp tarName $ \tmpFile handle -> do -- <tmp>/<package_name>.sllar.tar.<rand>

        -- writing package contents
        hPutStr handle (BS.unpack tarBody)
        hClose handle

        -- extracting data from tar
        extract tmpDir tmpFile

        -- examining sllar file existence
        files <- getDirectoryContents tmpDir
        let sllarFiles = filter (".sllar" `isInfixOf`) files

        if not . null $ sllarFiles
           then do
             -- check version and name availability in *.sllar
             sllarFileData <- BS.readFile (unslice_path [tmpDir, head sllarFiles]) >>= packageInfo

             -- examining correctness of *.sllar file
             case sllarFileData of
               Just conf -> do

                 -- checking if version not available
                 let (confName, confVersion) = (name conf, version conf)
                     packageName = confName ++ "-" ++ confVersion ++ ext
                     package = unslice_path [packages, confName, packageName]

                 versionAvailable <- doesFileExist package

                 if not versionAvailable
                   then do
                     -- check if parent folder available, create if not,
                     -- and copy file into package holder
                     let packageFolder = unslice_path [packages, confName]
                     doesPackageNameFolderExists <- doesDirectoryExist packageFolder
                     unless doesPackageNameFolderExists $ createDirectory packageFolder

                     copyFile tmpFile package
                     return "ok"

                   else return "version_exists"
               Nothing -> return "incorrect_config"
           else return "no_config"
