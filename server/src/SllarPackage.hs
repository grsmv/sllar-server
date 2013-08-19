{-# LANGUAGE DeriveGeneric #-}

-- This module provides support for reading YAML-based contents
-- of *.sllar files, that coming with all Sllar packages.
-- It's used during reading and verification of incoming package.

module SllarPackage where

import Data.Yaml
import GHC.Generics
import qualified Data.ByteString.Char8 as BS

data SllarPackage = SllarPackage
                  { name        :: String
                  , description :: String
                  , author      :: String
                  , version     :: String
                  } deriving (Show, Generic)

instance FromJSON SllarPackage


--
-- Presenting Sllar package information in system-readable
-- format
-- Input: ByteString with *.sllar file contents
-- Output: instance of type SllarPackage or nothing
--
packageInfo :: BS.ByteString -> IO (Maybe SllarPackage)
packageInfo s = return (decode s :: Maybe SllarPackage)
