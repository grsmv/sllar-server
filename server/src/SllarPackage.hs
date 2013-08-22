{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- This module provides support for reading YAML-based contents
-- of *.sllar files, that coming with all Sllar packages.
-- It's used during reading and verification of incoming package.

module SllarPackage where

import Data.Data
import Data.List
import Data.Yaml
import GHC.Generics
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.Utils as List
import qualified Data.String.Utils as String

-- Data type for presenting package essence, that comes FromJSON
-- user, that's why it differs from Package type, which is declared
-- in Package module. Main difference is in `version` field, instead
-- of `versions` in Package module. The difference exists because
-- package comes with only one (current) version from user.
data SllarPackage = SllarPackage
                  { name
                  , description
                  , author
                  , maintainer
                  , license
                  , copyright
                  , homepage
                  , tracker
                  , version :: Maybe String
                  } deriving (Data, Typeable, Show, Generic)

instance FromJSON SllarPackage


--
-- Presenting Sllar package information in system-readable
-- format
-- Input: ByteString with *.sllar file contents
-- Output: instance of type SllarPackage or nothing
--
packageInfo :: BS.ByteString -> IO (Maybe SllarPackage)
packageInfo s = return (decode (BS.pack (correct (BS.unpack s))) :: Maybe SllarPackage)


--
-- Getting type's fields
-- Output: type's fields as list of strings
--
defaultFields :: [String]
defaultFields = List.replace ["versions"] ["version"] fields
    where fields = constrFields . toConstr $ package

          -- TODO: change this crap
          package = SllarPackage e e e e e e e e e
          e = Just ""


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
