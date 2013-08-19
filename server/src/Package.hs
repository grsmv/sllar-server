{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Package (fromJson, toJson, publish) where

-- Sllar
import Paths_sllar_server
import qualified SllarPackage as SP

-- System
import Codec.Archive.Tar (extract)
import Control.Monad
import Data.Aeson
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import GHC.Generics
import System.IO (hPutStr, hClose)
import System.IO.Temp (withTempDirectory, withTempFile)
import System.Directory
import System.Path.NameManip (unslice_path)
import qualified Data.ByteString.Char8 as BS (pack, readFile, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64 as Base64 (decodeLenient)
import qualified Data.String.Utils as H -- part of MissingH package

data Package = Package
             { name
             , description
             , author
             , maintainer
             , license
             , copyright
             , homepage
             , tracker :: String
             , versions :: [Version]
             } deriving (Show, Generic)

instance ToJSON Package
instance FromJSON Package


data Version = Version
             { version :: String
             , date :: String }
             deriving (Show, Generic)

instance ToJSON Version
instance FromJSON Version


--
-- Processing raw string with JSON into parsed data
-- Input: string presentation of json
-- Output: parsed output
--
fromJson :: String -> Maybe [Package]
fromJson s = decode (BL.pack s) :: Maybe [Package]


--
-- Processing values of Package type into JSON
-- Input: list of packages
-- Output: json output
--
toJson :: [Package] -> BL.ByteString
toJson = encode


--
-- Recieving a package, testing it's correctness and storing it
-- Input: list of options as tuples
-- Output: action result
--
publish :: [(String, String)] -> IO String
publish options = do
    let value k = fromMaybe "" $ lookup k options
        tarName = value "tarName"
        tarBody = Base64.decodeLenient $ BS.pack (value "tarBody")
        ext = ".sllar.tar"
        tmpName = H.replace ext "" tarName

    tmp <- getDataFileName "tmp"
    packages <- getDataFileName "packages"

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
             sllarFileData <- BS.readFile (unslice_path [tmpDir, head sllarFiles]) >>= SP.packageInfo

             -- examining correctness of *.sllar file
             case sllarFileData of
               Just conf -> do

                 -- checking if version not available
                 let packageName = SP.name conf ++ "-" ++ SP.version conf ++ ext
                     package = unslice_path [packages, SP.name conf, packageName]

                 versionAvailable <- doesFileExist package

                 if not versionAvailable
                   then do
                     -- check if parent folder available, create if not,
                     -- and copy file into package holder
                     let packageFolder = unslice_path [packages, SP.name conf]
                     doesPackageNameFolderExists <- doesDirectoryExist packageFolder
                     unless doesPackageNameFolderExists $ createDirectory packageFolder

                     copyFile tmpFile package
                     return "ok"

                   else return "version_exists"
               Nothing -> return "incorrect_config"
           else return "no_config"
