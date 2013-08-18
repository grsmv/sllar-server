{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Package (fromJson, toJson, publish) where

-- Sllar
import Paths_sllar_server
import qualified SllarPackage as SP

-- System
import Codec.Archive.Tar (extract)
import Data.Aeson
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Yaml
import GHC.Generics
import System.IO.Temp
import System.Directory
import qualified Data.ByteString.Char8 as BS (pack, writeFile)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64 as Base64 (decodeLenient)
import qualified Data.String.Utils as H -- part of MissingH package

data Package = Package { name        :: String
                       , description :: String
                       , author      :: String
                       , maintainer  :: String
                       , license     :: String
                       , copyright   :: String
                       , homepage    :: String
                       , tracker     :: String
                       , versions    :: [Version]
                       } deriving (Show, Generic)

instance ToJSON Package
instance FromJSON Package


data Version = Version { version :: String, date :: String }
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
-- todo: tests
--         - is *.sllar file exists
--         - check version and name avalaibility in *.sllar
--         - check version and name equivalence with tar name
--
publish :: [(String, String)] -> IO String
publish options = do
    let value k = fromMaybe "" $ lookup k options
        tarName = value "tarName"
        tarBody = Base64.decodeLenient $ BS.pack (value "tarBody")
        tmpName = H.replace ".sllar.tar" "" tarName

    --
    -- ./tmp
    --   ├── package-0.0.1.sllar.tar
    --   └── package-0.0.1
    --       ├── package.sllar
    --       └── package.sl
    --

    -- unpacking and examining package
    tmp <- getDataFileName "tmp"
    return $ withTempDirectory tmp tmpName $ \tmpDir -> do

      -- extracting
      BS.writeFile (tmp ++ "/" ++ tarName) tarBody
      extract (tmp ++ "/" ++ tmpName) (tmp ++ "/" ++ tarName)

      -- examinig sllar file existence
      files <- getDirectoryContents (tmp ++ "/" ++ tmpName)
      let sllarFiles = filter (".sllar" `isInfixOf`) files

      return $
        if not . null $ sllarFiles
           then do
             -- check version and name availability in *.sllar
             sllarFileData <- BS.readFile (tmp ++ "/" ++ tmpName ++ "/" ++ head sllarFiles) >>= SP.packageInfo sllarFileContents
             case sllarFileData of
               Just sllarFileData' -> do

                 -- checking if version not available
                 packagesDir <- getDirectoryContents "packages"
                 let packagePath = packagesDir ++ "/" ++ SP.name sllarFileData' ++ "-" ++ SP.version sllarFileData' ++ ".sllar.tar"
                 versionAvailable <- doesFileExist packagePath
                 if not versionAvailable
                   then do
                     -- check if parent folder available, create if not
                     packageFolder <- getDataFileName $ "packages/" ++ SP.name sllarFileData'
                     doesPackageNameFolderExists <- doesDirectoryExist packageFolder
                     unless doesPackageNameFolderExists $ createDirectory packageFolder

                     -- copy file to package holder in `packages`
                     copyFile (tmp ++ "/" ++ tarName) packagePath

                     return "OK"

                   else "VERSION_AVAILABLE"

               Nothing -> "INCORRECT_SLLAR_FILE"

           else "NO_SLLAR_FILE"
