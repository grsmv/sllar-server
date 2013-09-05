{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS

data Person = Person
            { name
            , surname :: String
            , occupation :: Maybe String
            } deriving (Show)

instance FromJSON Person where
    parseJSON (Object v) =
      Person <$>
      (v .: "name") <*>
      (v .: "surname") <*>
      (v .:? "occupation")

fromJson :: BS.ByteString -> Maybe Person
fromJson str = decode str :: Maybe Person

main :: IO ()
main = do
    data' <- BS.readFile "missing_fields.json"
    print $ fromJson data'
