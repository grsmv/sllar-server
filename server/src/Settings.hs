module Settings (setting) where

data Setting = Setting { k :: String, v :: String } deriving Show


-- | Predefiled settings
settings :: [Setting]
settings = [ Setting "port" "5002"
           , Setting "tmp"  "/tmp"
           , Setting "home" "/etc/sllar" ]


-- | Searching for a specific key through list of settings
setting :: String -- ^ key
             -> String -- ^ value
setting key = v . head $ filter (\s -> k s == key) settings
