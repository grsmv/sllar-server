module ConfigReader (search, parse) where

import Data.List.Split
import qualified Data.Text as T

--
-- Searching among list of config key-value pairs
--
search :: String -> [(String, String)] -> Maybe String
search key values =
    let result = filter (\(k, _) -> key == k) values in
    case length result of
      0 -> Nothing
      _ -> Just (snd $ head result)


--
-- Reading YAML-like config and processing a list of key-value pairs
-- Input: raw string
-- Output: list of key-value pairs
--
parse :: String -> [(String, String)]
parse str =
    map (\l -> (strp (head (spl l)), strp (last $ spl l))) $ lines str
    where strp n = T.unpack . T.strip $ T.pack n
          spl = splitOn ":"
