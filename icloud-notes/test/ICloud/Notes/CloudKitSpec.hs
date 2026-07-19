{-# LANGUAGE OverloadedStrings #-}

module ICloud.Notes.CloudKitSpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Network.ICloud.Internal.Notes.CloudKit
  ( CKField (..)
  , CKLookupResponse (..)
  , CKQueryResponse (..)
  , CKRecord (..)
  , CKRecordRef (..)
  , CKZoneChangesResponse (..)
  , CKZoneChangesZone (..)
  )
import Test.Hspec


spec :: Spec
spec = describe "Network.ICloud.Internal.Notes.CloudKit" $ do
  describe "CKQueryResponse" $ do
    it "parses a folders query response" $ do
      case eitherDecode queryFoldersJson :: Either String CKQueryResponse of
        Left err -> expectationFailure err
        Right r -> do
          qrContinuationMarker r `shouldBe` Nothing
          case qrRecords r of
            [] -> expectationFailure "expected non-empty records"
            rec : _ -> do
              crName rec `shouldBe` "Folder/FOLDER-FIXTURE"
              crType rec `shouldBe` Just "SearchIndexes"
              Map.lookup "HasSubfolder" (crFields rec)
                `shouldBe` Just (CKInt64Field 1)
    it "parses an empty query response" $ do
      case eitherDecode emptyQueryJson :: Either String CKQueryResponse of
        Left err -> expectationFailure err
        Right r -> qrRecords r `shouldBe` []

  describe "CKLookupResponse" $ do
    it "parses a note lookup response" $ do
      case eitherDecode lookupNoteJson :: Either String CKLookupResponse of
        Left err -> expectationFailure err
        Right r -> do
          lrSyncToken r `shouldBe` Just "notes-lookup-sync-token-fixture"
          case lrRecords r of
            [] -> expectationFailure "expected non-empty records"
            rec : _ -> do
              crName rec `shouldBe` "Note/NOTE-FIXTURE"
              Map.lookup "TextDataEncrypted" (crFields rec)
                `shouldBe` Just (CKEncryptedBytesField "c3ludGhldGljIG5vdGUgYm9keQ==")
              Map.lookup "Folder" (crFields rec)
                `shouldBe` Just (CKReferenceField (mkRef "Folder/FOLDER-FIXTURE"))
              Map.lookup "Attachments" (crFields rec)
                `shouldBe` Just (CKReferenceListField [mkRef "Attachment/ATTACHMENT-FIXTURE"])
    it "parses an attachment lookup response" $ do
      case eitherDecode lookupAttachmentJson :: Either String CKLookupResponse of
        Left err -> expectationFailure err
        Right r -> do
          lrSyncToken r `shouldBe` Nothing
          case lrRecords r of
            [] -> expectationFailure "expected non-empty records"
            rec : _ -> do
              crName rec `shouldBe` "Attachment/ATTACHMENT-FIXTURE"
              Map.lookup "AttachmentUTI" (crFields rec)
                `shouldBe` Just (CKStringField "public.url")
              Map.lookup "Size" (crFields rec)
                `shouldBe` Just (CKInt64Field 128)

  describe "CKZoneChangesResponse" $ do
    it "parses zone changes with records" $ do
      case eitherDecode zoneChangesJson :: Either String CKZoneChangesResponse of
        Left err -> expectationFailure err
        Right r ->
          case zcrZones r of
            [] -> expectationFailure "expected non-empty zones"
            zone : _ -> do
              zczSyncToken zone `shouldBe` Just "notes-zone-sync-token-fixture"
              zczMoreComing zone `shouldBe` Just False
              case zczRecords zone of
                [_, _, deleted] -> do
                  crName deleted `shouldBe` "Note/NOTE-DELETED-FIXTURE"
                  crDeleted deleted `shouldBe` Just True
                recs -> expectationFailure $ "expected 3 records, got " <> show (length recs)
    it "parses zone changes with no records" $ do
      case eitherDecode zoneChangesEmptyJson :: Either String CKZoneChangesResponse of
        Left err -> expectationFailure err
        Right r ->
          case zcrZones r of
            [] -> expectationFailure "expected non-empty zones"
            zone : _ -> do
              zczRecords zone `shouldBe` []
              zczSyncToken zone `shouldBe` Just "notes-changes-sync-token-fixture"


-- Helpers

mkRef :: Text -> CKRecordRef
mkRef name = CKRecordRef{rrRecordName = name, rrAction = "VALIDATE"}


-- Fixtures

queryFoldersJson :: LBS.ByteString
queryFoldersJson =
  "{\"records\":[{\"recordName\":\"Folder/FOLDER-FIXTURE\"\
  \,\"recordType\":\"SearchIndexes\"\
  \,\"recordChangeTag\":\"folder-change-tag-fixture\"\
  \,\"zoneID\":{\"zoneName\":\"Notes\",\"zoneType\":\"REGULAR_CUSTOM_ZONE\"}\
  \,\"fields\":{\"TitleEncrypted\":{\"type\":\"STRING\",\"value\":\"Synthetic Folder\",\"isEncrypted\":true}\
  \,\"HasSubfolder\":{\"type\":\"INT64\",\"value\":1}}}]\
  \,\"continuationMarker\":null}"


emptyQueryJson :: LBS.ByteString
emptyQueryJson = "{\"records\":[]}"


lookupNoteJson :: LBS.ByteString
lookupNoteJson =
  "{\"records\":[{\"recordName\":\"Note/NOTE-FIXTURE\"\
  \,\"recordType\":\"Note\"\
  \,\"recordChangeTag\":\"note-change-tag-fixture\"\
  \,\"created\":{\"timestamp\":1735689600000,\"userRecordName\":\"_synthetic_user\"}\
  \,\"modified\":{\"timestamp\":1735776000000,\"userRecordName\":\"_synthetic_user\"}\
  \,\"deleted\":false\
  \,\"zoneID\":{\"zoneName\":\"Notes\",\"zoneType\":\"REGULAR_CUSTOM_ZONE\"}\
  \,\"fields\":{\
  \\"TitleEncrypted\":{\"type\":\"STRING\",\"value\":\"Synthetic note\",\"isEncrypted\":true}\
  \,\"SnippetEncrypted\":{\"type\":\"STRING\",\"value\":\"Synthetic snippet\",\"isEncrypted\":true}\
  \,\"TextDataEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"c3ludGhldGljIG5vdGUgYm9keQ==\"}\
  \,\"ModificationDate\":{\"type\":\"TIMESTAMP\",\"value\":1735776000000}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}\
  \,\"Folder\":{\"type\":\"REFERENCE\",\"value\":{\"recordName\":\"Folder/FOLDER-FIXTURE\",\"action\":\"VALIDATE\"}}\
  \,\"Attachments\":{\"type\":\"REFERENCE_LIST\",\"value\":[{\"recordName\":\"Attachment/ATTACHMENT-FIXTURE\",\"action\":\"VALIDATE\"}]}\
  \}}]\
  \,\"syncToken\":\"notes-lookup-sync-token-fixture\"}"


lookupAttachmentJson :: LBS.ByteString
lookupAttachmentJson =
  "{\"records\":[{\"recordName\":\"Attachment/ATTACHMENT-FIXTURE\"\
  \,\"recordType\":\"Attachment\"\
  \,\"recordChangeTag\":\"attachment-change-tag-fixture\"\
  \,\"zoneID\":{\"zoneName\":\"Notes\",\"zoneType\":\"REGULAR_CUSTOM_ZONE\"}\
  \,\"fields\":{\
  \\"AttachmentIdentifier\":{\"type\":\"STRING\",\"value\":\"ATTACHMENT-ALIAS-FIXTURE\"}\
  \,\"AttachmentUTI\":{\"type\":\"STRING\",\"value\":\"public.url\"}\
  \,\"Filename\":{\"type\":\"STRING\",\"value\":\"synthetic-link.webloc\"}\
  \,\"Size\":{\"type\":\"INT64\",\"value\":128}\
  \,\"PrimaryAsset\":{\"type\":\"ASSETID\",\"value\":{\
  \\"downloadURL\":\"https://example.test/notes/asset\"\
  \,\"fileChecksum\":\"notes-asset-checksum-fixture\"\
  \,\"referenceChecksum\":\"notes-asset-reference-fixture\"\
  \,\"wrappingKey\":\"notes-asset-wrapping-key-fixture\"\
  \,\"size\":128}}}}]}"


zoneChangesJson :: LBS.ByteString
zoneChangesJson =
  "{\"zones\":[{\"zoneID\":{\"zoneName\":\"Notes\",\"zoneType\":\"REGULAR_CUSTOM_ZONE\"}\
  \,\"syncToken\":\"notes-zone-sync-token-fixture\"\
  \,\"moreComing\":false\
  \,\"records\":[\
  \{\"recordName\":\"Note/NOTE-FIXTURE\"\
  \,\"recordType\":\"Note\"\
  \,\"recordChangeTag\":\"note-change-tag-fixture\"\
  \,\"fields\":{\
  \\"TitleEncrypted\":{\"type\":\"STRING\",\"value\":\"Synthetic note\",\"isEncrypted\":true}\
  \,\"SnippetEncrypted\":{\"type\":\"STRING\",\"value\":\"Synthetic snippet\",\"isEncrypted\":true}\
  \,\"ModificationDate\":{\"type\":\"TIMESTAMP\",\"value\":1735776000000}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}\
  \,\"Folder\":{\"type\":\"REFERENCE\",\"value\":{\"recordName\":\"Folder/FOLDER-FIXTURE\",\"action\":\"VALIDATE\"}}}}\
  \,{\"recordName\":\"Folder/FOLDER-FIXTURE\"\
  \,\"recordType\":\"SearchIndexes\"\
  \,\"recordChangeTag\":\"folder-change-tag-fixture\"\
  \,\"fields\":{\
  \\"TitleEncrypted\":{\"type\":\"STRING\",\"value\":\"Synthetic Folder\",\"isEncrypted\":true}\
  \,\"HasSubfolder\":{\"type\":\"INT64\",\"value\":1}}}\
  \,{\"recordName\":\"Note/NOTE-DELETED-FIXTURE\",\"deleted\":true}\
  \]}]}"


zoneChangesEmptyJson :: LBS.ByteString
zoneChangesEmptyJson =
  "{\"zones\":[{\"zoneID\":{\"zoneName\":\"Notes\",\"zoneType\":\"REGULAR_CUSTOM_ZONE\"}\
  \,\"records\":[]\
  \,\"syncToken\":\"notes-changes-sync-token-fixture\"\
  \,\"moreComing\":false}]}"
