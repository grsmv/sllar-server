{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExposedPackage
    ( allJson
    , ExposedPackage(..)
    , Version(..)) where

-- sllar
import Database

-- system
import Data.Maybe (fromMaybe)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Database.SQLite as SQLite

data ExposedPackage = ExposedPackage
                    { packageId :: Int
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

data Version = Version
             { version
             , uploadedAt :: String }
             deriving (Show, Generic)

instance ToJSON ExposedPackage
instance ToJSON Version


--
-- Processing values of Package type into JSON
-- Input: list of packages
-- Output: json output
--
allJson :: IO String
allJson = do
    packages <- all'
    return $ BL.unpack (encode packages)


--
-- Getting all registered packages
-- Output: list of values of ExposedPackage type
-- TODO: decompose this function
--       (versions getting, formatting a value of ExposedPackage type)
--
all' :: IO [ExposedPackage]
all' =
    withConnection $ \h -> do
        ls <- SQLite.execStatement h "select * from packages order by name"

        case ls :: Either String [[SQLite.Row SQLite.Value]] of
            Right [rows] ->
                do let extractInt (SQLite.Int a) = a
                       extractText (SQLite.Text a) = a

                       -- looking up for value of a specific type in a record (i - Int, t - Text)
                       i k = extractInt . fromMaybe (SQLite.Int 0) . lookup k
                       t k = extractText . fromMaybe (SQLite.Text "") . lookup k
                       pId row = fromIntegral $ i "id" row

                       -------------------------------------------------

                       getVersionsFor :: Int -> IO [Version]
                       getVersionsFor packageId' = do
                           versionsQuery <- SQLite.execStatement h $ "select * from versions where package_id = " ++ show packageId' ++ " order by uploaded_at"
                           let produceVersionsFrom row = Version { version = t "version" row
                                                                 , uploadedAt = t "uploaded_at" row}

                           case versionsQuery :: Either String [[SQLite.Row SQLite.Value]] of
                               Right [rows'] ->
                                   return $ map produceVersionsFrom rows'

                       -------------------------------------------------

                       producePackageFrom row = do
                       packageVersions <- getVersionsFor $ pId row
                       return ExposedPackage { packageId   = pId row
                                             , name        = t "name" row
                                             , description = t "description" row
                                             , author      = t "author" row
                                             , maintainer  = t "maintainer" row
                                             , license     = t "license" row
                                             , copyright   = t "copyright" row
                                             , homepage    = t "homepage" row
                                             , tracker     = t "tracker" row
                                             , versions    = packageVersions }

                   mapM producePackageFrom rows
