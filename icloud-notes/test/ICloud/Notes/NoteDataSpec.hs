{-# LANGUAGE OverloadedStrings #-}

module ICloud.Notes.NoteDataSpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.ICloud.Internal.Notes.CloudKit
  ( CKLookupResponse (..)
  , CKQueryResponse (..)
  , CKZoneChangesResponse (..)
  , CKZoneChangesZone (..)
  )
import Network.ICloud.Internal.Notes.NoteData
  ( noteRecordToFolder
  , noteRecordToNote
  , noteRecordToSummary
  , parseFoldersFromChanges
  , parseFoldersFromQuery
  , parseSummariesFromChanges
  , parseSummariesFromQuery
  )
import Network.ICloud.Notes.Note
import Test.Hspec


spec :: Spec
spec = describe "Network.ICloud.Internal.Notes.NoteData" $ do
  describe "noteRecordToSummary" $ do
    it "parses a note record into a NoteSummary" $ do
      case eitherDecode lookupNoteJson :: Either String CKLookupResponse of
        Left err -> expectationFailure err
        Right r -> case lrRecords r of
          [] -> expectationFailure "expected records"
          rec : _ ->
            noteRecordToSummary rec
              `shouldBe` Just
                NoteSummary
                  { nsId = NoteId "Note/NOTE-FIXTURE"
                  , nsTitle = Just "Synthetic note"
                  , nsSnippet = Just "Synthetic snippet"
                  , nsModified = Just (posixSecondsToUTCTime 1735776000)
                  , nsFolderId = Just (FolderId "Folder/FOLDER-FIXTURE")
                  , nsDeleted = False
                  , nsLocked = False
                  }
    it "returns Nothing for a tombstone" $ do
      case eitherDecode zoneChangesJson :: Either String CKZoneChangesResponse of
        Left err -> expectationFailure err
        Right r -> case concatMap zczRecords (zcrZones r) of
          [_, _, tombstone] -> noteRecordToSummary tombstone `shouldBe` Nothing
          recs -> expectationFailure $ "expected 3 records, got " <> show (length recs)
    it "returns Nothing for a folder record" $ do
      case eitherDecode queryFoldersJson :: Either String CKQueryResponse of
        Left err -> expectationFailure err
        Right r -> case qrRecords r of
          [] -> expectationFailure "expected records"
          rec : _ -> noteRecordToSummary rec `shouldBe` Nothing

  describe "noteRecordToFolder" $ do
    it "parses a folder record into a NoteFolder" $ do
      case eitherDecode queryFoldersJson :: Either String CKQueryResponse of
        Left err -> expectationFailure err
        Right r -> case qrRecords r of
          [] -> expectationFailure "expected records"
          rec : _ ->
            noteRecordToFolder rec
              `shouldBe` Just
                NoteFolder
                  { nfId = FolderId "Folder/FOLDER-FIXTURE"
                  , nfName = Just "Synthetic Folder"
                  }
    it "returns Nothing for a note record" $ do
      case eitherDecode lookupNoteJson :: Either String CKLookupResponse of
        Left err -> expectationFailure err
        Right r -> case lrRecords r of
          [] -> expectationFailure "expected records"
          rec : _ -> noteRecordToFolder rec `shouldBe` Nothing

  describe "noteRecordToNote" $ do
    it "decodes the note body from TextDataEncrypted" $ do
      case eitherDecode lookupNoteJson :: Either String CKLookupResponse of
        Left err -> expectationFailure err
        Right r -> case lrRecords r of
          [] -> expectationFailure "expected records"
          rec : _ -> case noteRecordToNote rec of
            Nothing -> expectationFailure "expected Just Note"
            Just n -> noteBodyBytes n `shouldBe` "synthetic note body"
    it "returns Nothing when TextDataEncrypted is absent" $ do
      case eitherDecode zoneChangesJson :: Either String CKZoneChangesResponse of
        Left err -> expectationFailure err
        Right r -> case concatMap zczRecords (zcrZones r) of
          noteRec : _ -> noteRecordToNote noteRec `shouldBe` Nothing
          [] -> expectationFailure "expected records"

  describe "parseSummariesFromQuery" $ do
    it "returns one summary from a note query response" $ do
      case eitherDecode queryNotesJson :: Either String CKQueryResponse of
        Left err -> expectationFailure err
        Right r -> length (parseSummariesFromQuery r) `shouldBe` 1
    it "returns empty from a folders-only response" $ do
      case eitherDecode queryFoldersJson :: Either String CKQueryResponse of
        Left err -> expectationFailure err
        Right r -> parseSummariesFromQuery r `shouldBe` []

  describe "parseFoldersFromQuery" $ do
    it "returns one folder from a folders query response" $ do
      case eitherDecode queryFoldersJson :: Either String CKQueryResponse of
        Left err -> expectationFailure err
        Right r -> length (parseFoldersFromQuery r) `shouldBe` 1
    it "returns empty from a notes-only response" $ do
      case eitherDecode queryNotesJson :: Either String CKQueryResponse of
        Left err -> expectationFailure err
        Right r -> parseFoldersFromQuery r `shouldBe` []

  describe "parseSummariesFromChanges" $ do
    it "extracts only Note records, skipping tombstones and folders" $ do
      case eitherDecode zoneChangesJson :: Either String CKZoneChangesResponse of
        Left err -> expectationFailure err
        Right r -> length (parseSummariesFromChanges r) `shouldBe` 1

  describe "parseFoldersFromChanges" $ do
    it "extracts only Folder records" $ do
      case eitherDecode zoneChangesJson :: Either String CKZoneChangesResponse of
        Left err -> expectationFailure err
        Right r -> length (parseFoldersFromChanges r) `shouldBe` 1


-- Fixtures

queryFoldersJson :: LBS.ByteString
queryFoldersJson =
  "{\"records\":[{\"recordName\":\"Folder/FOLDER-FIXTURE\"\
  \,\"recordType\":\"Folder\"\
  \,\"recordChangeTag\":\"folder-change-tag-fixture\"\
  \,\"zoneID\":{\"zoneName\":\"Notes\",\"zoneType\":\"REGULAR_CUSTOM_ZONE\"}\
  \,\"fields\":{\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIEZvbGRlcg==\"}\
  \,\"HasSubfolder\":{\"type\":\"INT64\",\"value\":1}}}]\
  \,\"continuationMarker\":null}"


queryNotesJson :: LBS.ByteString
queryNotesJson =
  "{\"records\":[{\"recordName\":\"Note/NOTE-FIXTURE\"\
  \,\"recordType\":\"Note\"\
  \,\"fields\":{\
  \\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIG5vdGU=\"}\
  \,\"SnippetEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIHNuaXBwZXQ=\"}\
  \,\"ModificationDate\":{\"type\":\"TIMESTAMP\",\"value\":1735776000000}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}\
  \,\"Folder\":{\"type\":\"REFERENCE\",\"value\":{\"recordName\":\"Folder/FOLDER-FIXTURE\",\"action\":\"VALIDATE\"}}}}]\
  \,\"continuationMarker\":null}"


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
  \\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIG5vdGU=\"}\
  \,\"SnippetEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIHNuaXBwZXQ=\"}\
  \,\"TextDataEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"c3ludGhldGljIG5vdGUgYm9keQ==\"}\
  \,\"ModificationDate\":{\"type\":\"TIMESTAMP\",\"value\":1735776000000}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}\
  \,\"Folder\":{\"type\":\"REFERENCE\",\"value\":{\"recordName\":\"Folder/FOLDER-FIXTURE\",\"action\":\"VALIDATE\"}}\
  \,\"Attachments\":{\"type\":\"REFERENCE_LIST\",\"value\":[{\"recordName\":\"Attachment/ATTACHMENT-FIXTURE\",\"action\":\"VALIDATE\"}]}\
  \}}]\
  \,\"syncToken\":\"notes-lookup-sync-token-fixture\"}"


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
  \\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIG5vdGU=\"}\
  \,\"SnippetEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIHNuaXBwZXQ=\"}\
  \,\"ModificationDate\":{\"type\":\"TIMESTAMP\",\"value\":1735776000000}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}\
  \,\"Folder\":{\"type\":\"REFERENCE\",\"value\":{\"recordName\":\"Folder/FOLDER-FIXTURE\",\"action\":\"VALIDATE\"}}}}\
  \,{\"recordName\":\"Folder/FOLDER-FIXTURE\"\
  \,\"recordType\":\"Folder\"\
  \,\"recordChangeTag\":\"folder-change-tag-fixture\"\
  \,\"fields\":{\
  \\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIEZvbGRlcg==\"}\
  \,\"HasSubfolder\":{\"type\":\"INT64\",\"value\":1}}}\
  \,{\"recordName\":\"Note/NOTE-DELETED-FIXTURE\",\"deleted\":true}\
  \]}]}"
