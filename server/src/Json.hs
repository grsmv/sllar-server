{-# LANGUAGE OverloadedStrings #-}

module Json where

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


jsonRead :: String -> Maybe [Package]
jsonRead s = decode (BL.pack s) :: Maybe [Package]

jsonWrite :: [Package] -> BL.ByteString
jsonWrite = encode


main :: IO ()
main = do
    -- s <- readFile "packages.json"
    -- let req = jsonRead s
    -- print req

    let reply = [
          Package "sexp" "another ose S-expr parser" "me" "Me" "MIT" "(c)" "http://github.com" "bugzilla" [Version "0.1" "12 Jun 2012"],
          Package "chlmrs" "Chalmers" "" "" "CHL" "" "" "Jira" [Version "0.1" "12 Jun 2012", Version "0.2" "09 Aug 2012"] ]
    BL.putStrLn (jsonWrite reply)
