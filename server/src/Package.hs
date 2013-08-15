{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Package (fromJson, toJson, publish) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL

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
--
--
publish :: [(String, String)] -> IO String
publish options = return "OK"
