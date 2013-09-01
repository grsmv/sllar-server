{-# LANGUAGE OverloadedStrings #-}

module ExternalPackage
  ( ExternalPackage(..)
  , Version(..) ) where

-- import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
-- import Control.Applicative ((<$>), (<*>))
-- import qualified Data.ByteString.Lazy.Char8 as BS

data ExternalPackage = ExternalPackage
                     { name
                     , description
                     , author
                     , maintainer
                     , license
                     , copyright
                     , homepage
                     , tracker
                     , repository :: String
                     , installed :: Bool
                     , versions :: [Version]
                     } deriving (Show)

data Version = Version
             { version
             , uploadedAt :: String
             } deriving (Show)
