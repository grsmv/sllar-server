module Main where

import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Text.Template
import Data.Maybe (fromMaybe)

main :: IO ()
main =
  S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
    where
        context assocs x = fromMaybe (T.pack "") $ lookup x assocs
        helloTemplate = T.pack "Hello, $name!\n"
        helloContext = context [(T.pack "name", T.pack "Joe")]
