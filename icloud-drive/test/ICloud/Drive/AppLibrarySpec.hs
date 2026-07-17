{-# LANGUAGE OverloadedStrings #-}

module ICloud.Drive.AppLibrarySpec (spec) where

import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.ICloud.Internal.Drive.Node
  ( AppLibrary (..)
  , AppLibraryIcon (..)
  , BundleId (..)
  )
import Network.ICloud.Internal.Drive.NodeData (parseAppLibrariesResponse)
import Test.Hspec


spec :: Spec
spec = describe "Network.ICloud.Internal.Drive.NodeData" $ do
  describe "parseAppLibrariesResponse" $ do
    it "returns one item per JSON entry" $ do
      v <- decodeOrFail twoItemsJson
      case parseEither parseAppLibrariesResponse v of
        Left err -> expectationFailure err
        Right libs -> length libs `shouldBe` 2

    it "extracts BundleId from docwsid" $ do
      v <- decodeOrFail twoItemsJson
      case parseEither parseAppLibrariesResponse v of
        Left err -> expectationFailure err
        Right (lib : _) ->
          alBundleId lib `shouldBe` BundleId "iCloud.is.workflow.my.workflows"
        Right [] -> expectationFailure "expected non-empty list"

    it "extracts optional name when present" $ do
      v <- decodeOrFail twoItemsJson
      case parseEither parseAppLibrariesResponse v of
        Left err -> expectationFailure err
        Right (lib : _) -> alName lib `shouldBe` Just "Shortcuts"
        Right [] -> expectationFailure "expected non-empty list"

    it "returns Nothing for absent name" $ do
      v <- decodeOrFail twoItemsJson
      case parseEither parseAppLibrariesResponse v of
        Left err -> expectationFailure err
        Right (_ : lib : _) -> alName lib `shouldBe` Nothing
        Right _ -> expectationFailure "expected at least two items"

    it "parses dateCreated timestamp" $ do
      v <- decodeOrFail twoItemsJson
      case parseEither parseAppLibrariesResponse v of
        Left err -> expectationFailure err
        Right (lib : _) ->
          alDateCreated lib `shouldBe` parseTs "2020-01-31T09:20:56Z"
        Right [] -> expectationFailure "expected non-empty list"

    it "parses icons when present" $ do
      v <- decodeOrFail twoItemsJson
      case parseEither parseAppLibrariesResponse v of
        Left err -> expectationFailure err
        Right (lib : _) ->
          case alIcons lib of
            [icon] -> do
              aliIconType icon `shouldBe` "IOS"
              aliSize icon `shouldBe` 120
            icons -> expectationFailure $ "expected 1 icon, got " <> show (length icons)
        Right [] -> expectationFailure "expected non-empty list"

    it "returns empty icon list when icons absent" $ do
      v <- decodeOrFail twoItemsJson
      case parseEither parseAppLibrariesResponse v of
        Left err -> expectationFailure err
        Right (_ : lib : _) -> alIcons lib `shouldBe` []
        Right _ -> expectationFailure "expected at least two items"

    it "fails when docwsid is absent" $ do
      v <- decodeOrFail missingDocwsidJson
      case parseEither parseAppLibrariesResponse v of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected parse failure"


-- Fixtures

twoItemsJson :: LBS.ByteString
twoItemsJson =
  "{\"items\":[\
  \{\"dateCreated\":\"2020-01-31T09:20:56Z\"\
  \,\"drivewsid\":\"FOLDER::com.apple.CloudDocs::appDocuments_iCloud.is.workflow.my.workflows\"\
  \,\"docwsid\":\"appDocuments_iCloud.is.workflow.my.workflows\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"Shortcuts\"\
  \,\"type\":\"APP_LIBRARY\"\
  \,\"maxDepth\":\"ANY\"\
  \,\"supportedTypes\":[\"com.apple.shortcut\"]\
  \,\"icons\":[{\"url\":\"https://example.icloud.com/icon120\",\"type\":\"IOS\",\"size\":120}]\
  \}\
  \,{\"dateCreated\":\"2015-10-04T16:12:16Z\"\
  \,\"drivewsid\":\"FOLDER::com.apple.CloudDocs::appDocuments_com.apple.mobilemail\"\
  \,\"docwsid\":\"appDocuments_com.apple.mobilemail\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"type\":\"APP_LIBRARY\"\
  \,\"maxDepth\":\"ANY\"\
  \,\"supportedTypes\":[\"public.item\",\"com.apple.mail.email\"]\
  \}\
  \]}"


missingDocwsidJson :: LBS.ByteString
missingDocwsidJson =
  "{\"items\":[{\"dateCreated\":\"2020-01-31T09:20:56Z\"\
  \,\"drivewsid\":\"FOLDER::com.apple.CloudDocs::appDocuments_iCloud.is.workflow.my.workflows\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"Shortcuts\"\
  \,\"type\":\"APP_LIBRARY\"\
  \,\"maxDepth\":\"ANY\"\
  \,\"supportedTypes\":[\"com.apple.shortcut\"]\
  \}]}"


-- Helpers

decodeOrFail :: LBS.ByteString -> IO Value
decodeOrFail bs = case eitherDecode bs of
  Left err -> fail err
  Right v -> pure v


parseTs :: String -> UTCTime
parseTs s = case (iso8601ParseM s :: Maybe UTCTime) of
  Just t -> t
  Nothing -> error $ "invalid timestamp: " <> s
