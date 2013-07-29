{-# LANGUAGE OverloadedStrings #-}

module Json (jsonRead, jsonWrite) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
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
                       } deriving Show

data Version = Version { version :: String, date :: String }
               deriving Show

instance ToJSON Package where
    toJSON (Package nameV descriptionV authorV maintainerV licenseV copyrightV homepageV trackerV versionsV) =
      object [ "name"        .= nameV
             , "description" .= descriptionV
             , "author"      .= authorV
             , "maintainer"  .= maintainerV
             , "license"     .= licenseV
             , "copyright"   .= copyrightV
             , "homepage"    .= homepageV
             , "tracker"     .= trackerV
             , "versions"    .= versionsV ]

instance ToJSON Version where
    toJSON (Version versionV dateV) = object [ "version" .= versionV
                                             , "date"    .= dateV ]

instance FromJSON Package where
    parseJSON (Object o) = Package <$>
                           o .: "name" <*>
                           o .: "description" <*>
                           o .: "author" <*>
                           o .: "maintainer" <*>
                           o .: "license" <*>
                           o .: "copyright" <*>
                           o .: "homepage" <*>
                           o .: "tracker" <*>
                           o .: "versions"
    parseJSON _          = empty

instance FromJSON Version where
    parseJSON (Object o) = Version <$>
                           o .: "version" <*>
                           o .: "date"
    parseJSON _          = empty


-- | Processing raw string with JSON into parsed data
jsonRead :: String          -- ^ string presentation of json
         -> Maybe [Package] -- ^ parsed output
jsonRead s = decode (BL.pack s) :: Maybe [Package]


-- | Processing values of Package type into JSON
jsonWrite :: [Package]     -- ^ list of packages
          -> BL.ByteString -- ^ json output
jsonWrite = encode
