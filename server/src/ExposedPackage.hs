{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExposedPackage (toJson) where

-- System
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL

data ExposedPackage = ExposedPackage
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

instance ToJSON ExposedPackage

data Version = Version
             { version :: String
             , date :: String }
             deriving (Show, Generic)

instance ToJSON Version

--
-- Processing values of Package type into JSON
-- Input: list of packages
-- Output: json output
--
toJson :: [ExposedPackage] -> BL.ByteString
toJson = encode
