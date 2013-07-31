module Settings (setting) where

data Setting = Setting { k :: String, v :: String } deriving Show


--
-- Predefiled settings
-- Output: list of settings
--
settings :: [Setting]
settings = [ Setting "port" "5002"
           , Setting "tmp"  "/tmp"
           , Setting "home" "/Users/sergey/Desktop/sllar/server" ]


--
-- Searching for a specific key through list of settings
-- TODO: change output type to Maybe String
-- Input: key to search
-- Output: founded value
--
setting :: String -> String
setting key = v . head $ filter (\s -> k s == key) settings
