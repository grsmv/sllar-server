module Main where

import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as E

import Data.Text.Template
import Data.Maybe (fromMaybe)

-- Create contet from associations list
context :: [(T.Text, T.Text)] -> Context
context assocs x = fromMaybe (err id) $ lookup x assocs
  where err = error $ "Could not fuind key: " ++ T.unpack x

main :: IO ()
main = S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
  where
    helloTemplate = T.pack "Hello, $name!\n"
    helloContext = context [(T.pack "name", T.pack "Joe")]
