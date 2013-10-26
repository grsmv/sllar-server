{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

-- This module provides support for reading YAML-based contents
-- of *.sllar files, that coming with all Sllar packages.
-- It's used during reading and verification of incoming package.

module Package
    ( Package(..)
    , info
    , toTuple
    , publish
    , defaultFields
    , unusedFields
    , correct ) where

-- Sllar
import Database
import qualified Paths_sllar_server as Paths

-- System
import Codec.Archive.Tar (extract)
import Control.Monad
import Data.Data
import Data.DateTime (getCurrentTime)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Yaml
import GHC.Generics
import qualified Data.ByteString.Base64 as Base64 (decodeLenient)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Generics as G
import qualified Data.List.Utils as List
import qualified Data.String.Utils as String
import qualified Database.SQLite as SQLite
import System.Directory
import System.IO (hPutStr, hClose)
import System.IO.Temp (withTempDirectory, withTempFile)
import System.Path.NameManip (unslice_path)

data Package = Package
             -- mandatory fields
             { name
             , description
             , author
             , version :: String

             -- optional fields
             , maintainer
             , license
             , copyright
             , homepage
             , tracker :: Maybe String
             } deriving (Data, Typeable, Show, Generic, Eq)

instance FromJSON Package


--
-- Presenting Sllar package information in system-readable format
-- Input: ByteString with *.sllar file contents
-- Output: instance of type Package or nothing
--
info :: BS.ByteString -> IO (Maybe Package)
info s = return (decode (BS.pack (correct (BS.unpack s))) :: Maybe Package)


--
-- Getting type's fields
-- Output: type's fields as list of strings
-- todo: remove
--
defaultFields :: [String]
defaultFields = List.replace ["versions"] ["version"] fields
    where fields = constrFields . toConstr $ package
          package = Package "" "" "" "" j j j j j  -- todo: fix this crap
          j = Just ""


--
-- Analyzing raw data from YAML file and detecting fields,
-- that not present in YAML file (why?)
-- Input: data, all fields thar should be presented
-- Output: fields, that not presented in input data
-- todo: remove
--
unusedFields :: String -> [String] -> [String]
unusedFields str =
    filter (\e -> not $ (e ++ ":") `isInfixOf` String.replace " " "" str)


--
-- Corecting data in YAML file (adding fields that not presented,
-- but which should be presented).
-- Input: raw data from YAML files
-- Output: corrected data
-- todo: remove
--
correct :: String -> String
correct text = foldl (\acc e -> acc ++ e ++ ": \n") text fields
               where fields = unusedFields text defaultFields


--
-- Convering value of Package to tuple
-- Input: value of Package
-- Output: list of key-value tuples, representing contents of package
--
toTuple :: Package -> [(String, String)]
toTuple pkg =
      map (\f -> (f, get f :: String)) mandatory ++
      map (\f -> (f, fromMaybe "" (get f :: Maybe String))) (fields \\ mandatory)
      where mandatory = ["name", "description", "author", "version"] -- todo: detect this dynamically
            get f = getField f pkg
            fields = constrFields . toConstr $ pkg

            -- Getting contents of a record field by field name presented as String
            getField :: (Data r, Typeable v) => String -> r -> v
            getField fieldName rec = gmapQi i (e `G.extQ` id) rec
                where i = fieldName `fieldIndex` rec
                      e _ = error "type mismatch"

                      -- Order in range of record's fields
                      fieldIndex :: (Data r) => String -> r -> Int
                      fieldIndex fieldName' rec' =
                          fromMaybe 0 $ fieldName' `elemIndex` (constrFields . toConstr $ rec')


--
-- Recieving a package, testing it's correctness and storing it
-- Input: list of options as tuples
-- Output: action result
-- todo: move most of definitions of paths to a separate function
--
publish :: [(String, String)] -> IO String
publish options = do
    let value k = fromMaybe "" $ lookup k options
        tarName = value "tarName"
        tarBody = Base64.decodeLenient $ BS.pack (value "tarBody")
        ext = ".sllar.tar"
        tmpName = String.replace ext "" tarName

    tmp <- Paths.getDataFileName "tmp"
    packages <- Paths.getDataFileName "packages"

    -- unpacking and examining package
    withTempDirectory tmp tmpName $ \tmpDir ->         -- <tmp>/<package_name>.<rand>
      withTempFile tmp tarName $ \tmpFile handle -> do -- <tmp>/<package_name>.sllar.tar.<rand>

        -- writing package contents
        hPutStr handle (BS.unpack tarBody)
        hClose handle

        -- extracting data from tar
        extract tmpDir tmpFile

        -- examining sllar file existence
        files <- getDirectoryContents tmpDir
        let sllarFiles = filter (".sllar" `isInfixOf`) files

        if not . null $ sllarFiles
           then do
             -- check version and name availability in *.sllar
             sllarFileData <- BS.readFile (unslice_path [tmpDir, head sllarFiles]) >>= info

             -- examining correctness of *.sllar file
             case sllarFileData of
               Just conf -> do

                 -- checking if version not available
                 let (confName, confVersion) = (name conf, version conf)
                     packageName = confName ++ "-" ++ confVersion ++ ext
                     package = unslice_path [packages, confName, packageName]

                 versionAvailable <- doesFileExist package

                 if not versionAvailable
                   then do
                     -- check if parent folder available, create if not,
                     -- and copy file into package holder
                     let packageFolder = unslice_path [packages, confName]
                     doesPackageNameFolderExists <- doesDirectoryExist packageFolder
                     unless doesPackageNameFolderExists $ createDirectory packageFolder

                     copyFile tmpFile package
                     savePackageInDatabase conf
                     return "ok"

                   else return "version_exists"
               Nothing -> return "incorrect_config"
           else return "no_config"



--
-- Creating package in the Database, or updatimg Package version if package already
-- presented in the database
-- Input: Package value
-- todo: report errors during ratabase update
--
savePackageInDatabase :: Package -> IO ()
savePackageInDatabase pkg = withConnection $ \h -> do

    currentDateTime <- getCurrentTime

    let packageInfo = Package.toTuple pkg

        -- updatimf `versions` table with info from package
        createVersion packageId =
          SQLite.insertRow h "versions" [
              ("version",     fromMaybe "" $ lookup "version" packageInfo),
              ("package_id",  show packageId),
              ("uploaded_at", show currentDateTime)]

    ls <- SQLite.execStatement h $ "select id from packages where name='" ++ Package.name pkg ++ "'"

    case ls :: Either String [[SQLite.Row SQLite.Value]] of
        Right [row] ->

            -- check if package already presented with lower version
            case length row of

                -- if not exists - create package and version
                0 -> do SQLite.insertRow h "packages" (filter (\(k, _) -> k /= "version") packageInfo)
                        rowId <- SQLite.getLastRowID h
                        createVersion rowId

                -- otherwise - create version
                _ -> do let Just (SQLite.Int id') = lookup "id" $ head row
                        createVersion id'

    return ()
