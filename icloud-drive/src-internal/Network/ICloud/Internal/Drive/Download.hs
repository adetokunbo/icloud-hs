{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.ICloud.Internal.Drive.Download
  ( fetchNode
  , fetchChildren
  , fetchFile
  , fetchAppLibraries
  , fetchAppLibrariesRaw
  , execCreateFolder
  , execRenameNode
  , execDeleteNode
  , execUploadFile
  )
where

import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client
  ( Request
  , RequestBody (..)
  , Response (..)
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  )
import Network.HTTP.Types (hContentType, methodPost, statusCode)
import Network.ICloud.Http (Api, rawRequest)
import Network.ICloud.Internal.Drive.Endpoints
  ( CloudScope
  , DriveEndpoints
  , appLibrariesReq
  , commitUploadReq
  , createFolderBody
  , createFolderReq
  , deleteNodeBody
  , deleteNodeReq
  , downloadTokenReq
  , nodeDetailsBody
  , nodeDetailsReq
  , renameNodeBody
  , renameNodeReq
  , uploadTokenReq
  )
import Network.ICloud.Internal.Drive.Node
  ( AppLibrary
  , DriveNode
  , DriveNodeId
  , FileData (..)
  , FolderData (..)
  , folderDocId
  , nodeEtag
  , nodeId
  )
import Network.ICloud.Internal.Drive.NodeData
  ( UploadReceipt (..)
  , parseAppLibrariesResponse
  , parseChildrenResponse
  , parseDownloadUrl
  , parseNodeResponse
  , parseUploadReceiptResponse
  , parseUploadTokenResponse
  )


-- | Fetch metadata for a single node.
fetchNode :: Api -> DriveEndpoints s -> DriveNodeId -> IO DriveNode
fetchNode api ep nid = do
  resp <- rawRequest api (nodeReq ep nid)
  checkStatus "fetchNode" resp
  body <- decodeBody "fetchNode" resp
  either fail pure $ parseEither parseNodeResponse body


-- | Fetch the immediate children of a folder.
fetchChildren :: Api -> DriveEndpoints s -> DriveNodeId -> IO [DriveNode]
fetchChildren api ep nid = do
  resp <- rawRequest api (nodeReq ep nid)
  checkStatus "fetchChildren" resp
  body <- decodeBody "fetchChildren" resp
  either fail pure $ parseEither parseChildrenResponse body


-- | Download the contents of a file node as a lazy 'LBS.ByteString'.
fetchFile :: Api -> DriveEndpoints s -> FileData -> IO LBS.ByteString
fetchFile api ep fd
  | fdSize fd == Nothing = pure LBS.empty
  | otherwise = do
      tokenResp <- rawRequest api (downloadTokenReq (fdDocId fd) (fdZone fd) ep)
      checkStatus "fetchFile (token)" tokenResp
      tokenBody <- decodeBody "fetchFile (token)" tokenResp
      url <- either fail pure $ parseEither parseDownloadUrl tokenBody
      contentReq <- getReqFromUrl url
      contentResp <- rawRequest api contentReq
      checkStatus "fetchFile (content)" contentResp
      pure $ responseBody contentResp


-- | Fetch and parse the @retrieveAppLibraries@ response as @[AppLibrary]@.
fetchAppLibraries :: Api -> DriveEndpoints s -> IO [AppLibrary]
fetchAppLibraries api ep = do
  resp <- rawRequest api (appLibrariesReq ep)
  checkStatus "fetchAppLibraries" resp
  body <- decodeBody "fetchAppLibraries" resp
  either fail pure $ parseEither parseAppLibrariesResponse body


-- | Fetch the raw JSON body from @GET retrieveAppLibraries@.
fetchAppLibrariesRaw :: Api -> DriveEndpoints s -> IO LBS.ByteString
fetchAppLibrariesRaw api ep = do
  resp <- rawRequest api (appLibrariesReq ep)
  checkStatus "fetchAppLibrariesRaw" resp
  pure $ responseBody resp


-- | Create a new folder under the given parent node.
execCreateFolder :: Api -> DriveEndpoints CloudScope -> DriveNodeId -> Text -> IO ()
execCreateFolder api ep parentId name = do
  resp <- rawRequest api req
  checkStatus "createFolder" resp
 where
  req =
    (createFolderReq ep)
      { requestBody = RequestBodyLBS (createFolderBody ep parentId name)
      , requestHeaders = (hContentType, "application/json") : requestHeaders (createFolderReq ep)
      }


-- | Rename a drive node (folder or file).
execRenameNode :: Api -> DriveEndpoints CloudScope -> DriveNode -> Text -> IO ()
execRenameNode api ep node name = do
  resp <- rawRequest api req
  checkStatus "renameNode" resp
 where
  req =
    (renameNodeReq ep)
      { requestBody = RequestBodyLBS (renameNodeBody (nodeId node) (nodeEtag node) name)
      , requestHeaders = (hContentType, "application/json") : requestHeaders (renameNodeReq ep)
      }


-- | Move a drive node (folder or file) to the trash.
execDeleteNode :: Api -> DriveEndpoints CloudScope -> DriveNode -> IO ()
execDeleteNode api ep node = do
  resp <- rawRequest api req
  checkStatus "deleteNode" resp
 where
  req =
    (deleteNodeReq ep)
      { requestBody = RequestBodyLBS (deleteNodeBody ep (nodeId node) (nodeEtag node))
      , requestHeaders = (hContentType, "application/json") : requestHeaders (deleteNodeReq ep)
      }


{- | Upload file content into a folder using the 3-step iCloud Drive upload
protocol.
-}
execUploadFile :: Api -> DriveEndpoints CloudScope -> FolderData -> Text -> LBS.ByteString -> IO ()
execUploadFile api ep folder filename content = do
  let zone = fnZone folder
      tokenBody = uploadTokenBodyBytes filename (LBS.length content)
      tokenReq' =
        (uploadTokenReq zone ep)
          { requestBody = RequestBodyLBS tokenBody
          , requestHeaders = (hContentType, "text/plain") : requestHeaders (uploadTokenReq zone ep)
          }
  tokenResp <- rawRequest api tokenReq'
  checkStatus "uploadFile (token)" tokenResp
  tokenVal <- decodeBody "uploadFile (token)" tokenResp
  (docId, uploadUrl) <- either fail pure $ parseEither parseUploadTokenResponse tokenVal
  uploadReq <- buildUploadReq filename content uploadUrl
  uploadResp <- rawRequest api uploadReq
  checkStatus "uploadFile (content)" uploadResp
  uploadVal <- decodeBody "uploadFile (content)" uploadResp
  receipt <- either fail pure $ parseEither parseUploadReceiptResponse uploadVal
  nowMs <- currentTimeMs
  let fDocId = folderDocId folder
      commitBody = buildCommitBody docId fDocId filename receipt nowMs
      commitReq' =
        (commitUploadReq zone ep)
          { requestBody = RequestBodyLBS commitBody
          , requestHeaders = (hContentType, "text/plain") : requestHeaders (commitUploadReq zone ep)
          }
  commitResp <- rawRequest api commitReq'
  checkStatus "uploadFile (commit)" commitResp


uploadTokenBodyBytes :: Text -> Int64 -> LBS.ByteString
uploadTokenBodyBytes filename size =
  encode $
    object
      [ "filename" .= filename
      , "type" .= ("FILE" :: Text)
      , "content_type" .= ("" :: Text)
      , "size" .= size
      ]


buildUploadReq :: Text -> LBS.ByteString -> Text -> IO Request
buildUploadReq filename content url = do
  req <- getReqFromUrl url
  let body = buildMultipartBody filename content
      ct = "multipart/form-data; boundary=" <> uploadBoundary
  pure
    req
      { requestBody = RequestBodyLBS body
      , requestHeaders = (hContentType, ct) : requestHeaders req
      , method = methodPost
      }


buildMultipartBody :: Text -> LBS.ByteString -> LBS.ByteString
buildMultipartBody filename content =
  let fn = LBS.fromStrict (BS8.pack (Text.unpack filename))
      bd = LBS.fromStrict ("--" <> uploadBoundary)
   in bd
        <> "\r\nContent-Disposition: form-data; name=\""
        <> fn
        <> "\"; filename=\""
        <> fn
        <> "\"\r\nContent-Type: application/octet-stream\r\n\r\n"
        <> content
        <> "\r\n"
        <> bd
        <> "--\r\n"


buildCommitBody :: Text -> Text -> Text -> UploadReceipt -> Int64 -> LBS.ByteString
buildCommitBody docId folderId filename receipt nowMs =
  encode $
    object
      [ "data" .= dataObj
      , "command" .= ("add_file" :: Text)
      , "create_short_guid" .= True
      , "document_id" .= docId
      , "path"
          .= object
            [ "starting_document_id" .= folderId
            , "path" .= filename
            ]
      , "allow_conflict" .= True
      , "file_flags"
          .= object
            [ "is_writable" .= True
            , "is_executable" .= False
            , "is_hidden" .= False
            ]
      , "mtime" .= nowMs
      , "btime" .= nowMs
      ]
 where
  dataObj = object $ baseData ++ receiptField
  baseData =
    [ "signature" .= urFileChecksum receipt
    , "wrapping_key" .= urWrappingKey receipt
    , "reference_signature" .= urReferenceChecksum receipt
    , "size" .= urSize receipt
    ]
  receiptField = case urReceipt receipt of
    Nothing -> []
    Just r -> ["receipt" .= r]


currentTimeMs :: IO Int64
currentTimeMs = do
  t <- getPOSIXTime
  pure $ round (t * 1000)


uploadBoundary :: BS8.ByteString
uploadBoundary = "WebKitFormBoundaryicloud"


nodeReq :: DriveEndpoints s -> DriveNodeId -> Request
nodeReq ep nid =
  (nodeDetailsReq ep)
    { requestBody = RequestBodyLBS (nodeDetailsBody nid)
    , requestHeaders =
        (hContentType, "application/json") : requestHeaders (nodeDetailsReq ep)
    }


getReqFromUrl :: Text -> IO Request
getReqFromUrl = parseRequest . Text.unpack


checkStatus :: String -> Response a -> IO ()
checkStatus ctx resp =
  let code = statusCode (responseStatus resp)
   in if code >= 400
        then fail $ ctx <> ": HTTP " <> show code
        else pure ()


decodeBody :: String -> Response LBS.ByteString -> IO Value
decodeBody ctx resp =
  case eitherDecode (responseBody resp) of
    Left err -> fail $ ctx <> ": JSON decode error: " <> err
    Right v -> pure v
