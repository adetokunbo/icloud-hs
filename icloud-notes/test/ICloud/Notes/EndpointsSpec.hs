{-# LANGUAGE OverloadedStrings #-}

module ICloud.Notes.EndpointsSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Network.HTTP.Client (Request (..))
import Network.HTTP.Types (methodPost)
import Network.ICloud.Internal.Notes.Endpoints
  ( changesBody
  , changesReq
  , foldersBody
  , lookupBody
  , lookupReq
  , mkNotesEndpoints
  , queryReq
  , recentsBody
  )
import Network.ICloud.Session (AccountData (..), Credentials (..), Session (..))
import Test.Hspec


spec :: Spec
spec = describe "Network.ICloud.Internal.Notes.Endpoints" $ do
  ep <- runIO (mkNotesEndpoints testAccountData testSession)

  describe "queryReq" $ do
    it "targets /records/query" $
      path (queryReq ep) `shouldSatisfy` BS.isSuffixOf "/records/query"
    it "uses POST" $
      method (queryReq ep) `shouldBe` methodPost
    it "includes remapEnums query param" $
      queryString (queryReq ep) `shouldSatisfy` BS.isInfixOf "remapEnums=true"
    it "includes getCurrentSyncToken query param" $
      queryString (queryReq ep) `shouldSatisfy` BS.isInfixOf "getCurrentSyncToken=true"
    it "includes clientId query param" $
      queryString (queryReq ep) `shouldSatisfy` BS.isInfixOf "clientId=auth-test-client-id"

  describe "lookupReq" $ do
    it "targets /records/lookup" $
      path (lookupReq ep) `shouldSatisfy` BS.isSuffixOf "/records/lookup"
    it "uses POST" $
      method (lookupReq ep) `shouldBe` methodPost
    it "includes clientId query param" $
      queryString (lookupReq ep) `shouldSatisfy` BS.isInfixOf "clientId=auth-test-client-id"

  describe "changesReq" $ do
    it "targets /changes/zone" $
      path (changesReq ep) `shouldSatisfy` BS.isSuffixOf "/changes/zone"
    it "uses POST" $
      method (changesReq ep) `shouldBe` methodPost
    it "includes clientId query param" $
      queryString (changesReq ep) `shouldSatisfy` BS.isInfixOf "clientId=auth-test-client-id"

  describe "foldersBody" $ do
    it "queries SearchIndexes with parentless filter" $
      foldersBody 10 Nothing
        `shouldSatisfy` lbsContains "\"recordType\":\"SearchIndexes\""
    it "includes Notes zoneID with zoneType" $
      foldersBody 10 Nothing
        `shouldSatisfy` lbsContains "\"zoneType\":\"REGULAR_CUSTOM_ZONE\""
    it "includes the requested resultsLimit" $
      foldersBody 50 Nothing
        `shouldSatisfy` lbsContains "\"resultsLimit\":50"
    it "clamps resultsLimit to 200" $
      foldersBody 999 Nothing
        `shouldSatisfy` lbsContains "\"resultsLimit\":200"
    it "includes continuationMarker when provided" $
      foldersBody 10 (Just (String "test-marker"))
        `shouldSatisfy` lbsContains "\"continuationMarker\""

  describe "recentsBody" $ do
    it "queries Note records" $
      recentsBody 10 Nothing
        `shouldSatisfy` lbsContains "\"recordType\":\"Note\""
    it "includes Notes zoneID with zoneType" $
      recentsBody 10 Nothing
        `shouldSatisfy` lbsContains "\"zoneType\":\"REGULAR_CUSTOM_ZONE\""
    it "includes the requested resultsLimit" $
      recentsBody 42 Nothing
        `shouldSatisfy` lbsContains "\"resultsLimit\":42"
    it "clamps resultsLimit to 200" $
      recentsBody 999 Nothing
        `shouldSatisfy` lbsContains "\"resultsLimit\":200"
    it "sorts by modTime descending" $
      recentsBody 10 Nothing
        `shouldSatisfy` lbsContains "\"fieldName\":\"modTime\""

  describe "lookupBody" $ do
    it "includes each record name" $
      lookupBody ["Note/ABC", "Note/DEF"]
        `shouldSatisfy` lbsContains "\"recordName\":\"Note/ABC\""
    it "includes Notes zoneID with zoneType" $
      lookupBody ["Note/ABC"]
        `shouldSatisfy` lbsContains "\"zoneType\":\"REGULAR_CUSTOM_ZONE\""

  describe "changesBody" $ do
    it "includes REGULAR_CUSTOM_ZONE zoneType" $
      changesBody Nothing
        `shouldSatisfy` lbsContains "\"zoneType\":\"REGULAR_CUSTOM_ZONE\""
    it "requests Note record type" $
      changesBody Nothing
        `shouldSatisfy` lbsContains "\"Note\""
    it "includes syncToken when provided" $
      changesBody (Just "test-sync-token")
        `shouldSatisfy` lbsContains "\"syncToken\":\"test-sync-token\""


-- Helpers

lbsContains :: BS.ByteString -> LBS.ByteString -> Bool
lbsContains needle = BS.isInfixOf needle . LBS.toStrict


-- Fixtures

testAccountData :: AccountData
testAccountData =
  AccountData
    { adHsaVersion = 2
    , adHsaChallengeRequired = False
    , adHsaTrustedBrowser = True
    , adWebservices =
        Map.fromList
          [("ckdatabasews", "https://p31-ckdatabasews.icloud.com")]
    }


testSession :: Session
testSession =
  Session
    { sessionCreds = Credentials{credAccountName = "test@example.com", credPassword = "test-pass"}
    , sessionTopDir = "/tmp/test"
    , sessionClientId = "auth-test-client-id"
    }
