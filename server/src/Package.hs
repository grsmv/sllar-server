{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- todo: rename this module to "PackageInformation"
-- todo: rename type to "PackageInformation"

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
