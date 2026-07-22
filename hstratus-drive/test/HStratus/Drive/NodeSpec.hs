{-# LANGUAGE OverloadedStrings #-}

module HStratus.Drive.NodeSpec (spec) where

import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.HStratus.Internal.Drive.Node
  ( DriveNode (..)
  , DriveNodeId (..)
  , FileData (..)
  , FolderData (..)
  )
import Network.HStratus.Internal.Drive.NodeData
  ( parseChildrenResponse
  , parseDownloadUrl
  , parseNodeResponse
  )
import Test.Hspec


spec :: Spec
spec = describe "Network.HStratus.Internal.Drive.NodeData" $ do
  describe "parseNodeResponse" $ do
    it "parses a root folder" $ do
      v <- decodeOrFail rootFolderJson
      parseEither parseNodeResponse v
        `shouldBe` Right (DriveFolder rootFolderData)
    it "treats APP_LIBRARY type as DriveFolder" $ do
      v <- decodeOrFail appLibraryJson
      case parseEither parseNodeResponse v of
        Left err -> expectationFailure err
        Right (DriveFile _) -> expectationFailure "expected DriveFolder for APP_LIBRARY"
        Right (DriveFolder fd) ->
          fnId fd `shouldBe` DriveNodeId "FOLDER::com.apple.Keynote::documents"

  describe "parseChildrenResponse" $ do
    it "returns empty list when items field is absent" $ do
      v <- decodeOrFail rootFolderJson
      parseEither parseChildrenResponse v `shouldBe` Right []
    it "returns all children" $ do
      v <- decodeOrFail subfolderJson
      case parseEither parseChildrenResponse v of
        Left err -> expectationFailure err
        Right nodes -> length nodes `shouldBe` 2
    it "parses file children with extension, size, and dateModified" $ do
      v <- decodeOrFail subfolderJson
      case parseEither parseChildrenResponse v of
        Left err -> expectationFailure err
        Right (DriveFile fd : _) -> do
          fdDocId fd `shouldBe` "33A41112-4131-4938-9691-7F356CE3C51D"
          fdExtension fd `shouldBe` Just "pdf"
          fdSize fd `shouldBe` Just 19876991
          fdDateModified fd `shouldBe` parseTs "2020-04-27T21:37:36Z"
        Right nodes ->
          expectationFailure $ "expected file as first child, got: " <> show nodes

  describe "parseDownloadUrl" $ do
    it "extracts url from data_token" $ do
      v <- decodeOrFail dataTokenJson
      parseEither parseDownloadUrl v `shouldBe` Right dataTokenUrl
    it "falls back to package_token when data_token is absent" $ do
      v <- decodeOrFail pkgTokenJson
      parseEither parseDownloadUrl v `shouldBe` Right pkgTokenUrl
    it "fails when neither token field is present" $ do
      v <- decodeOrFail "{\"document_id\":\"516C896C\"}"
      case parseEither parseDownloadUrl v of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected parse failure"


-- Fixtures

rootFolderData :: FolderData
rootFolderData =
  FolderData
    { fnId = DriveNodeId "FOLDER::com.apple.CloudDocs::root"
    , fnEtag = "31"
    , fnName = ""
    , fnZone = "com.apple.CloudDocs"
    , fnDateCreated = Nothing
    }


rootFolderJson :: LBS.ByteString
rootFolderJson =
  "[{\"drivewsid\":\"FOLDER::com.apple.CloudDocs::root\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"\"\
  \,\"etag\":\"31\"\
  \,\"type\":\"FOLDER\"\
  \}]"


appLibraryJson :: LBS.ByteString
appLibraryJson =
  "[{\"drivewsid\":\"FOLDER::com.apple.Keynote::documents\"\
  \,\"zone\":\"com.apple.Keynote\"\
  \,\"name\":\"Keynote\"\
  \,\"etag\":\"2m\"\
  \,\"type\":\"APP_LIBRARY\"\
  \,\"dateCreated\":\"2019-12-12T14:33:55-08:00\"\
  \}]"


subfolderJson :: LBS.ByteString
subfolderJson =
  "[{\"drivewsid\":\"FOLDER::com.apple.CloudDocs::D5AA0425-E84F-4501-AF5D-60F1D92648CF\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"Test\"\
  \,\"etag\":\"2z\"\
  \,\"type\":\"FOLDER\"\
  \,\"items\":[\
  \{\"drivewsid\":\"FILE::com.apple.CloudDocs::33A41112-4131-4938-9691-7F356CE3C51D\"\
  \,\"docwsid\":\"33A41112-4131-4938-9691-7F356CE3C51D\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"Scan 2\"\
  \,\"dateModified\":\"2020-04-27T21:37:36Z\"\
  \,\"size\":19876991\
  \,\"etag\":\"2k::2j\"\
  \,\"extension\":\"pdf\"\
  \,\"type\":\"FILE\"\
  \}\
  \,{\"drivewsid\":\"FILE::com.apple.CloudDocs::516C896C-6AA5-4A30-B30E-5502C2333DAE\"\
  \,\"docwsid\":\"516C896C-6AA5-4A30-B30E-5502C2333DAE\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"Scanned document 1\"\
  \,\"dateModified\":\"2020-05-03T00:15:17Z\"\
  \,\"size\":21644358\
  \,\"etag\":\"32::2x\"\
  \,\"extension\":\"pdf\"\
  \,\"type\":\"FILE\"\
  \}\
  \]}]"


dataTokenUrl :: Text
dataTokenUrl = "https://cvws.icloud-content.com/B/sig1ref1/Scanned+document+1.pdf?o=obj&v=1"


dataTokenJson :: LBS.ByteString
dataTokenJson =
  "{\"data_token\":{\"url\":\"https://cvws.icloud-content.com/B/sig1ref1/Scanned+document+1.pdf?o=obj&v=1\"\
  \,\"token\":\"tok1\"\
  \}}"


pkgTokenUrl :: Text
pkgTokenUrl = "https://cvws.icloud-content.com/B/sig2ref2/pkg.zip?o=obj&v=1"


pkgTokenJson :: LBS.ByteString
pkgTokenJson =
  "{\"package_token\":{\"url\":\"https://cvws.icloud-content.com/B/sig2ref2/pkg.zip?o=obj&v=1\"\
  \,\"token\":\"tok2\"\
  \}}"


-- Helpers

decodeOrFail :: LBS.ByteString -> IO Value
decodeOrFail bs = case eitherDecode bs of
  Left err -> fail err
  Right v -> pure v


parseTs :: String -> Maybe UTCTime
parseTs = iso8601ParseM
