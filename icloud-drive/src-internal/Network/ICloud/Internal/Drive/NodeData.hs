{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.ICloud.Internal.Drive.NodeData
  ( parseNodeResponse
  , parseChildrenResponse
  , parseDownloadUrl
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
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Vector as V
import Network.ICloud.Internal.Drive.Node
  ( DriveNode (..)
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


-- | Parse an ISO 8601 timestamp in either UTC (@Z@) or offset (@±HH:MM@) form.
parseTimestamp :: Text -> Parser UTCTime
parseTimestamp t =
  case iso8601ParseM (Text.unpack t) of
    Just ut -> pure ut
    Nothing -> fail $ "invalid ISO 8601 timestamp: " <> Text.unpack t
