{-# LANGUAGE DeriveGeneric #-}

module ConfigReader (repos) where

import Data.Yaml
import GHC.Generics
import qualified Data.ByteString.Char8 as BS

data Repositories = Repositories { repositories :: [String] }
                    deriving (Show, Generic)

instance FromJSON Repositories

--
-- Returning a list of repositories to get data from
-- Output: list of urls to repositories
--
repos :: IO (Maybe [String])
repos = do rawData <- readFile "example.yaml"
           let repos' = decode (BS.pack rawData) :: Maybe Repositories
           return $ case repos' of
             Just crds -> Just $ repositories crds
             _         -> Nothing
