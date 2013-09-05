-- concurrent: ./get_url > /dev/null  0.35s user 0.16s system 1% cpu 26.927 total
-- sequental: ./get_url > /dev/null  0.35s user 0.17s system 1% cpu 28.082 total
-- parallel: ./get_url > /dev/null  0.38s user 0.18s system 2% cpu 26.598 total

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit (simpleHttp)
import Control.Applicative ((<$>))
import Control.Monad ((>=>), join, liftM)
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as Lazy

pages :: IO [String]
pages = lines <$> readFile "links.txt"

-- todo: http error catching and displaying
-- todo: async data collecting

main :: IO ()
main = concurrent

sequental, concurrent :: IO ()
sequental =
  do pages' <- pages
     mapM_ (simpleHttp >=> Lazy.putStr) pages'

concurrent =
  do pages' <- pages
     mapM_ (\page -> do a <- async $ simpleHttp page
                        x <- wait a
                        Lazy.putStr x) pages'
