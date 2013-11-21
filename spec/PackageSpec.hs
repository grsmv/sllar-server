{-# LANGUAGE QuasiQuotes #-}

module Main where

import Sllar.Package.Import
import Sllar.Heredoc

import Test.Hspec
import qualified Data.ByteString.Char8 as BS

correctExample, incorrectExample :: String
correctExample = [heredoc|
    name: package
    description: example description
    author: me
    version: 0.0.1
|]

incorrectExample = [heredoc|
    name: package
    description: long story short
    author: me
|]

main :: IO ()
main = hspec $ do

    describe "Package.info" $ do
        it "should parse when all mandatory fields filled" $ do
            package <- info $ BS.pack correctExample
            package `shouldBe` Just Package { name = "package"
                                            , description = "example description"
                                            , author = "me"
                                            , version = "0.0.1"
                                            , maintainer = Nothing
                                            , license = Nothing
                                            , copyright = Nothing
                                            , tracker = Nothing
                                            , homepage = Nothing
                                            }

        it "shouldn't parse when not all mandatory fields filled" $ do
            package <- info $ BS.pack incorrectExample
            package `shouldBe` Nothing

    describe "Package.toTuple" $
        it "should convert Package to list of tuples" $ do
            package <- info $ BS.pack correctExample
            let Just pkg = package
            toTuple pkg `shouldBe` [ ("name",         "package")
                                   , ("description",  "example description")
                                   , ("author",       "me")
                                   , ("version",      "0.0.1")
                                   , ("maintainer",   "")
                                   , ("license",      "")
                                   , ("copyright",    "")
                                   , ("homepage",     "")
                                   , ("tracker",      "")
                                   ]
