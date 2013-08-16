{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Package (fromJson, toJson, publish) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import GHC.Generics
import qualified Data.ByteString.Char8 as BS (pack, writeFile)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64 as Base64 (decodeLenient)

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
    BS.writeFile ("/Users/sergey/Desktop/" ++ tarName) tarBody
    return "OK"
