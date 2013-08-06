{-# LANGUAGE DeriveGeneric #-}

module Config where

-- Sllar
import Paths_sllar_server

-- System
import Data.Yaml
import GHC.Generics
import qualified Data.ByteString.Char8 as BS

data Config = Config { port :: Integer } deriving (Show, Generic)

instance FromJSON Config

--
-- Returning a config
-- Output: config (or nothing)
--
config :: IO (Maybe Config)
config = do configPath <- getDataFileName "config"
            rawText <- readFile configPath
            let cfg = decode (BS.pack rawText) :: Maybe Config
            return cfg
