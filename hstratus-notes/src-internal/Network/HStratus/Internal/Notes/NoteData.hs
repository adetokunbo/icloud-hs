{-# LANGUAGE OverloadedStrings #-}

module Network.HStratus.Internal.Notes.NoteData
  ( noteRecordToSummary
  , noteRecordToFolder
  , noteRecordToNote
  , parseSummariesFromQuery
  , parseFoldersFromQuery
  , parseSummariesFromChanges
  , parseFoldersFromChanges
  )
where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Network.HStratus.Internal.Notes.CloudKit
  ( CKField (..)
  , CKQueryResponse (..)
  , CKRecord (..)
  , CKRecordRef (..)
  , CKZoneChangesResponse (..)
  , CKZoneChangesZone (..)
  , parseMillisTimestamp
  )
import Network.HStratus.Internal.Notes.Note
  ( FolderId (..)
  , Note (..)
  , NoteFolder (..)
  , NoteId (..)
  , NoteSummary (..)
  )


noteRecordToSummary :: CKRecord -> Maybe NoteSummary
noteRecordToSummary rec = do
  rt <- crType rec
  guard (rt == "Note" || rt == "PasswordProtectedNote")
  pure
    NoteSummary
      { nsId = NoteId (crName rec)
      , nsTitle = fieldEncryptedBytesAsText "TitleEncrypted" rec
      , nsSnippet = fieldEncryptedBytesAsText "SnippetEncrypted" rec
      , nsModified = fieldTimestamp "ModificationDate" rec
      , nsFolderId = fieldFolderId "Folder" rec
      , nsDeleted = maybe False (/= 0) (fieldInt64 "Deleted" rec)
      , nsLocked = rt == "PasswordProtectedNote"
      }


noteRecordToFolder :: CKRecord -> Maybe NoteFolder
noteRecordToFolder rec = do
  rt <- crType rec
  guard (rt == "Folder")
  pure
    NoteFolder
      { nfId = FolderId (crName rec)
      , nfName = fieldEncryptedBytesAsText "TitleEncrypted" rec
      }


noteRecordToNote :: CKRecord -> Maybe Note
noteRecordToNote rec = do
  summary <- noteRecordToSummary rec
  bodyText <- fieldEncryptedBytes "TextDataEncrypted" rec
  bodyBytes <- decodeBase64Text bodyText
  pure Note{noteInfo = summary, noteBodyBytes = bodyBytes}


parseSummariesFromQuery :: CKQueryResponse -> [NoteSummary]
parseSummariesFromQuery = mapMaybe noteRecordToSummary . qrRecords


parseFoldersFromQuery :: CKQueryResponse -> [NoteFolder]
parseFoldersFromQuery = mapMaybe noteRecordToFolder . qrRecords


parseSummariesFromChanges :: CKZoneChangesResponse -> [NoteSummary]
parseSummariesFromChanges = mapMaybe noteRecordToSummary . allZoneRecords


parseFoldersFromChanges :: CKZoneChangesResponse -> [NoteFolder]
parseFoldersFromChanges = mapMaybe noteRecordToFolder . allZoneRecords


-- Helpers

allZoneRecords :: CKZoneChangesResponse -> [CKRecord]
allZoneRecords = concatMap zczRecords . zcrZones


fieldInt64 :: Text -> CKRecord -> Maybe Int64
fieldInt64 key rec = case Map.lookup key (crFields rec) of
  Just (CKInt64Field i) -> Just i
  _ -> Nothing


fieldTimestamp :: Text -> CKRecord -> Maybe UTCTime
fieldTimestamp key rec = case Map.lookup key (crFields rec) of
  Just (CKTimestampField ms) -> Just (parseMillisTimestamp ms)
  _ -> Nothing


fieldFolderId :: Text -> CKRecord -> Maybe FolderId
fieldFolderId key rec = case Map.lookup key (crFields rec) of
  Just (CKReferenceField ref) -> Just (FolderId (rrRecordName ref))
  _ -> Nothing


fieldEncryptedBytes :: Text -> CKRecord -> Maybe Text
fieldEncryptedBytes key rec = case Map.lookup key (crFields rec) of
  Just (CKEncryptedBytesField t) -> Just t
  _ -> Nothing


-- Decode a base64-encoded ENCRYPTED_BYTES field to UTF-8 text.
-- For unprotected accounts, titles and snippets are plain UTF-8 in base64.
fieldEncryptedBytesAsText :: Text -> CKRecord -> Maybe Text
fieldEncryptedBytesAsText key rec = do
  b64 <- fieldEncryptedBytes key rec
  bs <- decodeBase64Text b64
  case TE.decodeUtf8' bs of
    Left _ -> Nothing
    Right t -> Just t


decodeBase64Text :: Text -> Maybe ByteString
decodeBase64Text t = case B64.decode (TE.encodeUtf8 t) of
  Left _ -> Nothing
  Right bs -> Just bs
