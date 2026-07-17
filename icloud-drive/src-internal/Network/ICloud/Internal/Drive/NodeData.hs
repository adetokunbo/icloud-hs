{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.ICloud.Internal.Drive.NodeData
  ( parseNodeResponse
  , parseChildrenResponse
  , parseDownloadUrl
  , parseAppLibrariesResponse
  , UploadReceipt (..)
  , parseUploadTokenResponse
  , parseUploadReceiptResponse
  )
where

import Data.Aeson
  ( Object
  , Value
  , withArray
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Vector as V
import Network.ICloud.Internal.Drive.Node
  ( AppLibrary (..)
  , AppLibraryIcon (..)
  , BundleId (..)
  , DriveNode (..)
  , DriveNodeId (..)
  , FileData (..)
  , FolderData (..)
  )


{- | Parse the first element of a @retrieveItemDetailsInFolders@ response as a
@DriveNode@.
-}
parseNodeResponse :: Value -> Parser DriveNode
parseNodeResponse = withArray "node response" $ \arr ->
  case V.toList arr of
    [] -> fail "retrieveItemDetailsInFolders: empty response array"
    (v : _) -> parseNode v


{- | Parse the children from the first element of a
@retrieveItemDetailsInFolders@ response.
-}
parseChildrenResponse :: Value -> Parser [DriveNode]
parseChildrenResponse = withArray "children response" $ \arr ->
  case V.toList arr of
    [] -> fail "retrieveItemDetailsInFolders: empty response array"
    (v : _) -> withObject "folder" parseItems v


-- | Extract the download URL from a @download/by_id@ response.
parseDownloadUrl :: Value -> Parser Text
parseDownloadUrl = withObject "download response" $ \o -> do
  dataToken <- o .:? "data_token"
  pkgToken <- o .:? "package_token"
  case (dataToken, pkgToken) of
    (Just dt, _) -> withObject "data_token" (.: "url") dt
    (_, Just pt) -> withObject "package_token" (.: "url") pt
    _ -> fail "download response: neither data_token nor package_token found"


parseNode :: Value -> Parser DriveNode
parseNode = withObject "DriveNode" $ \o -> do
  nodeType <- o .: "type" :: Parser Text
  case nodeType of
    "FILE" -> DriveFile <$> parseFileData o
    _ -> DriveFolder <$> parseFolderData o


parseFolderData :: Object -> Parser FolderData
parseFolderData o =
  FolderData
    <$> (DriveNodeId <$> o .: "drivewsid")
    <*> o .: "etag"
    <*> o .: "name"
    <*> o .: "zone"
    <*> (o .:? "dateCreated" >>= traverse parseTimestamp)


parseFileData :: Object -> Parser FileData
parseFileData o =
  FileData
    <$> (DriveNodeId <$> o .: "drivewsid")
    <*> o .: "docwsid"
    <*> o .: "etag"
    <*> o .: "name"
    <*> o .:? "extension"
    <*> o .: "zone"
    <*> (nothingIfZero <$> o .:? "size")
    <*> (o .:? "dateCreated" >>= traverse parseTimestamp)
    <*> (o .:? "dateModified" >>= traverse parseTimestamp)


parseItems :: Object -> Parser [DriveNode]
parseItems o = do
  items <- o .:? "items" >>= pure . fromMaybe []
  mapM parseNode items


nothingIfZero :: Maybe Int64 -> Maybe Int64
nothingIfZero (Just 0) = Nothing
nothingIfZero x = x


-- | Parse the @retrieveAppLibraries@ response body as a list of 'AppLibrary'.
parseAppLibrariesResponse :: Value -> Parser [AppLibrary]
parseAppLibrariesResponse = withObject "app libraries response" $ \o -> do
  items <- o .: "items"
  mapM parseAppLibrary items


parseAppLibrary :: Value -> Parser AppLibrary
parseAppLibrary = withObject "AppLibrary" $ \o -> do
  docwsid <- o .: "docwsid"
  bid <- parseBundleId docwsid
  name <- o .:? "name"
  dc <- o .: "dateCreated" >>= parseTimestamp
  rawIcons <- fromMaybe [] <$> o .:? "icons"
  icons <- mapM parseAppLibraryIcon rawIcons
  pure AppLibrary{alBundleId = bid, alName = name, alDateCreated = dc, alIcons = icons}


parseBundleId :: Text -> Parser BundleId
parseBundleId docwsid =
  case Text.stripPrefix "appDocuments_" docwsid of
    Just bid -> pure (BundleId bid)
    Nothing -> fail $ "AppLibrary: unexpected docwsid format: " <> Text.unpack docwsid


parseAppLibraryIcon :: Value -> Parser AppLibraryIcon
parseAppLibraryIcon = withObject "AppLibraryIcon" $ \o ->
  AppLibraryIcon
    <$> o .: "url"
    <*> o .: "type"
    <*> o .: "size"


-- | Checksum metadata returned after uploading file content (step 2 of upload).
data UploadReceipt = UploadReceipt
  { urFileChecksum :: !Text
  , urWrappingKey :: !Text
  , urReferenceChecksum :: !Text
  , urSize :: !Int64
  , urReceipt :: !(Maybe Text)
  }


-- | Parse the @upload/web@ response to extract @(document_id, upload_url)@.
parseUploadTokenResponse :: Value -> Parser (Text, Text)
parseUploadTokenResponse = withArray "upload token response" $ \arr ->
  case V.toList arr of
    [] -> fail "upload token response: empty array"
    (v : _) ->
      withObject "upload token" (\o -> (,) <$> o .: "document_id" <*> o .: "url") v


-- | Parse the multipart-upload response body to extract 'UploadReceipt'.
parseUploadReceiptResponse :: Value -> Parser UploadReceipt
parseUploadReceiptResponse = withObject "upload receipt response" $ \o -> do
  sf <- o .: "singleFile"
  withObject "singleFile" parseReceiptFields sf


parseReceiptFields :: Object -> Parser UploadReceipt
parseReceiptFields o =
  UploadReceipt
    <$> o .: "fileChecksum"
    <*> o .: "wrappingKey"
    <*> o .: "referenceChecksum"
    <*> o .: "size"
    <*> o .:? "receipt"


-- | Parse an ISO 8601 timestamp in either UTC (@Z@) or offset (@±HH:MM@) form.
parseTimestamp :: Text -> Parser UTCTime
parseTimestamp t =
  let s = Text.unpack t
   in case (iso8601ParseM s :: Maybe UTCTime) of
        Just ut -> pure ut
        Nothing -> case (iso8601ParseM s :: Maybe ZonedTime) of
          Just zt -> pure (zonedTimeToUTC zt)
          Nothing -> fail $ "invalid ISO 8601 timestamp: " <> s
