{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Network.HStratus.Internal.Notes.CloudKit
  ( CKZoneId (..)
  , CKRecordRef (..)
  , CKAsset (..)
  , CKTimestamp (..)
  , CKField (..)
  , CKRecord (..)
  , CKQueryResponse (..)
  , CKLookupResponse (..)
  , CKZoneChangesZone (..)
  , CKZoneChangesResponse (..)
  , parseMillisTimestamp
  )
where

import Control.Monad (guard)
import Data.Aeson
  ( FromJSON (..)
  , Object
  , Value
  , withObject
  , (.!=)
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import Data.Foldable (asum)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


parseMillisTimestamp :: Int64 -> UTCTime
parseMillisTimestamp ms = posixSecondsToUTCTime (fromIntegral ms / 1000)


data CKZoneId = CKZoneId
  { czName :: Text
  , czType :: Text
  }
  deriving (Eq, Show)


instance FromJSON CKZoneId where
  parseJSON = withObject "CKZoneId" $ \o ->
    CKZoneId
      <$> o .: "zoneName"
      <*> o .: "zoneType"


data CKRecordRef = CKRecordRef
  { rrRecordName :: Text
  , rrAction :: Text
  }
  deriving (Eq, Show)


instance FromJSON CKRecordRef where
  parseJSON = withObject "CKRecordRef" $ \o ->
    CKRecordRef
      <$> o .: "recordName"
      <*> o .: "action"


data CKAsset = CKAsset
  { caDownloadUrl :: Text
  , caFileChecksum :: Text
  , caRefChecksum :: Text
  , caWrappingKey :: Text
  , caSize :: Int64
  }
  deriving (Eq, Show)


instance FromJSON CKAsset where
  parseJSON = withObject "CKAsset" $ \o ->
    CKAsset
      <$> o .: "downloadURL"
      <*> o .: "fileChecksum"
      <*> o .: "referenceChecksum"
      <*> o .: "wrappingKey"
      <*> o .: "size"


data CKTimestamp = CKTimestamp
  { ctTimestamp :: Int64
  , ctUserRecordName :: Text
  }
  deriving (Eq, Show)


instance FromJSON CKTimestamp where
  parseJSON = withObject "CKTimestamp" $ \o ->
    CKTimestamp
      <$> o .: "timestamp"
      <*> o .: "userRecordName"


-- Internal newtypes: give distinct Haskell types to CK tags that share a
-- primitive (Text covers "STRING"/"ENCRYPTED_BYTES"; Int64 covers
-- "INT64"/"TIMESTAMP"), making CKFieldTag a total function.
newtype CKString = CKString Text
  deriving (FromJSON)


newtype CKEncryptedBytes = CKEncryptedBytes Text
  deriving (FromJSON)


newtype CKInt64Value = CKInt64Value Int64
  deriving (FromJSON)


newtype CKTimestampValue = CKTimestampValue Int64
  deriving (FromJSON)


-- Single source of truth mapping each value type to its CloudKit "type" tag.
type family CKFieldTag a :: Symbol where
  CKFieldTag CKString = "STRING"
  CKFieldTag CKInt64Value = "INT64"
  CKFieldTag CKTimestampValue = "TIMESTAMP"
  CKFieldTag CKEncryptedBytes = "ENCRYPTED_BYTES"
  CKFieldTag CKRecordRef = "REFERENCE"
  CKFieldTag [CKRecordRef] = "REFERENCE_LIST"
  CKFieldTag CKAsset = "ASSETID"


-- Confirms the pre-parsed "type" tag matches CKFieldTag a, then parses "value".
matchField
  :: forall a
   . (KnownSymbol (CKFieldTag a), FromJSON a)
  => Text
  -> Object
  -> Parser a
matchField typ o = do
  guard (typ == pack (symbolVal (Proxy :: Proxy (CKFieldTag a))))
  o .: "value"


data CKField
  = CKStringField Text
  | CKInt64Field Int64
  | CKTimestampField Int64
  | CKEncryptedBytesField Text
  | CKReferenceField CKRecordRef
  | CKReferenceListField [CKRecordRef]
  | CKAssetIdField CKAsset
  | CKUnknownField Text Value
  deriving (Eq, Show)


instance FromJSON CKField where
  parseJSON = withObject "CKField" $ \o -> do
    typ <- o .: "type" :: Parser Text
    asum
      [ CKStringField . (\(CKString t) -> t) <$> matchField typ o
      , CKInt64Field . (\(CKInt64Value i) -> i) <$> matchField typ o
      , CKTimestampField . (\(CKTimestampValue i) -> i) <$> matchField typ o
      , CKEncryptedBytesField . (\(CKEncryptedBytes t) -> t) <$> matchField typ o
      , CKReferenceField <$> matchField typ o
      , CKReferenceListField <$> matchField typ o
      , CKAssetIdField <$> matchField typ o
      , CKUnknownField typ <$> o .: "value"
      ]


data CKRecord = CKRecord
  { crName :: Text
  , crType :: Maybe Text
  , crChangeTag :: Maybe Text
  , crZoneId :: Maybe CKZoneId
  , crFields :: Map Text CKField
  , crCreated :: Maybe CKTimestamp
  , crModified :: Maybe CKTimestamp
  , crDeleted :: Maybe Bool
  }
  deriving (Eq, Show)


instance FromJSON CKRecord where
  parseJSON = withObject "CKRecord" $ \o ->
    CKRecord
      <$> o .: "recordName"
      <*> o .:? "recordType"
      <*> o .:? "recordChangeTag"
      <*> o .:? "zoneID"
      <*> o .:? "fields" .!= mempty
      <*> o .:? "created"
      <*> o .:? "modified"
      <*> o .:? "deleted"


data CKQueryResponse = CKQueryResponse
  { qrRecords :: [CKRecord]
  , qrContinuationMarker :: Maybe Value
  }
  deriving (Eq, Show)


instance FromJSON CKQueryResponse where
  parseJSON = withObject "CKQueryResponse" $ \o ->
    CKQueryResponse
      <$> o .: "records"
      <*> o .:? "continuationMarker"


data CKLookupResponse = CKLookupResponse
  { lrRecords :: [CKRecord]
  , lrSyncToken :: Maybe Text
  }
  deriving (Eq, Show)


instance FromJSON CKLookupResponse where
  parseJSON = withObject "CKLookupResponse" $ \o ->
    CKLookupResponse
      <$> o .: "records"
      <*> o .:? "syncToken"


data CKZoneChangesZone = CKZoneChangesZone
  { zczZoneId :: CKZoneId
  , zczSyncToken :: Maybe Text
  , zczMoreComing :: Maybe Bool
  , zczRecords :: [CKRecord]
  }
  deriving (Eq, Show)


instance FromJSON CKZoneChangesZone where
  parseJSON = withObject "CKZoneChangesZone" $ \o ->
    CKZoneChangesZone
      <$> o .: "zoneID"
      <*> o .:? "syncToken"
      <*> o .:? "moreComing"
      <*> o .:? "records" .!= []


newtype CKZoneChangesResponse = CKZoneChangesResponse
  { zcrZones :: [CKZoneChangesZone]
  }
  deriving (Eq, Show)


instance FromJSON CKZoneChangesResponse where
  parseJSON = withObject "CKZoneChangesResponse" $ \o ->
    CKZoneChangesResponse <$> o .: "zones"
