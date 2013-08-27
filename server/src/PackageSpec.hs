module PackageSpec where

import Package
import Test.Hspec
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = hspec $ do

    let exampleSllarFileContents = "name: Example\ndescription: Example\nauthor: me\nversion: 0.1.1\n"

    describe "Package.info" $ do
        it "should present Sllar package information in system-readable format if all mandatory fields filled" $ do
            package <- BS.readFile "../fixtures/example.yaml" >>= info
            package `shouldBe` Just Package{ name="package", description="example description",
                                             author="me", version="0.0.1", maintainer=Nothing,
                                             license=Nothing, copyright=Nothing, tracker=Nothing,
                                             homepage=Nothing }

        it "should return Nothing if one of mandatory fields not filled" $ do
            package <- BS.readFile "../fixtures/wrong_example.yaml" >>= info
            package `shouldBe` Nothing


    describe "Package.defaultFields" $
        it "should return fields of Package type" $
            defaultFields `shouldBe` ["name", "description", "author", "version", "maintainer", "license", "copyright", "homepage", "tracker"]

    describe "Package.unusedFields" $
        it "should return fields, that not used in Sllar file" $
            unusedFields exampleSllarFileContents defaultFields `shouldBe` ["maintainer", "license", "copyright", "homepage", "tracker"]

    describe "Package.correct" $
        it "should correct Sllar file" $
            correct exampleSllarFileContents `shouldBe` exampleSllarFileContents ++ "maintainer: \nlicense: \ncopyright: \nhomepage: \ntracker: \n"
