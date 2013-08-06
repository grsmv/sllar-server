{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Package (fromJSON, toJSON) where

import Control.Applicative ((<$>), (<*>), empty)
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
fromJSON :: String -> Maybe [Package]
fromJSON s = decode (BL.pack s) :: Maybe [Package]


--
-- Processing values of Package type into JSON
-- Input: list of packages
-- Output: json output
--
toJSON :: [Package] -> BL.ByteString
toJSON = encode
