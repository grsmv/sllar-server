{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExposedPackage
    ( allPackages
    , ExposedPackage(..)
    , Version(..)) where

-- sllar
import Database

-- system
import Data.Maybe (fromMaybe)
import Data.Aeson
import GHC.Generics
--import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Database.SQLite as SQLite

data ExposedPackage = ExposedPackage
                    { pId :: Int
                    , name
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
--toJson :: [ExposedPackage] -> BL.ByteString
--toJson = encode



--
--
--
allPackages :: IO () -- [ExposedPackage]
allPackages =
    withConnection $ \h -> do
        ls <- SQLite.execStatement h "select * from packages order by name"

        case ls :: Either String [[SQLite.Row SQLite.Value]] of
            Right [rows] -> do let x = map (unwrap . fromMaybe (SQLite.Int 0) . lookup "id") rows
                                   unwrap (SQLite.Int a) = a
                               print x

                                --lookup' k xs = fromMaybe "" (lookup k xs)
                                --Just SQLite.Text a = producePackage rows
                                --producePackage xs = ExposedPackage { pId=        SQLite.Int  (fromMaybe 0 (lookup "id" xs))
                                --                                   , name=       SQLite.Text (lookup' "name" xs)
                                --                                   , description=SQLite.Text (lookup' "description" xs)
                                --                                   , author=     SQLite.Text (lookup' "author" xs)
                                --                                   , maintainer= SQLite.Text (lookup' "maintainer" xs)
                                --                                   , license=    SQLite.Text (lookup' "license" xs)
                                --                                   , copyright=  SQLite.Text (lookup' "copyright" xs)
                                --                                   , homepage=   SQLite.Text (lookup' "homepage" xs)
                                --                                   , tracker=    SQLite.Text (lookup' "tracker" xs)
                                --                                   , versions=[] }

-- { pId=1, name="", description="", author="", maintainer="", license="", copyright="", homepage="", tracker="", versions=[] }
