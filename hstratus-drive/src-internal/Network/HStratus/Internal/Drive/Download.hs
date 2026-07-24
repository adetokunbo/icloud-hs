{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.HStratus.Internal.Drive.Download
  ( DriveError (..)
  , fetchNode
  , fetchChildren
  , fetchFile
  , execCreateFolder
  , execRenameNode
  , execDeleteNode
  , execUploadFile
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HStratus.Http (Api, rawRequest)
import Network.HStratus.Internal.Drive.Endpoints
  ( DriveEndpoints
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
import Network.HStratus.Internal.Drive.Node
  ( DriveNode (..)
  , DriveNodeId (..)
  , FileData (..)
  , FolderData (..)
  , folderDocId
  , nodeEtag
  , nodeId
  )
import Network.HStratus.Internal.Drive.NodeData
  ( UploadReceipt (..)
  , parseChildrenResponse
  , parseDownloadUrl
  , parseNodeResponse
  , parseUploadReceiptResponse
  , parseUploadTokenResponse
  )
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


data DriveError
  = DriveHttpError Int
  | DriveParseError String
  | DriveInvalidRoot


instance Show DriveError where
  show (DriveHttpError n) = "iCloud Drive: HTTP error " <> show n
  show (DriveParseError msg) = "iCloud Drive: parse error: " <> msg
  show DriveInvalidRoot = "iCloud Drive: invalid root node"


instance Exception DriveError


-- | Fetch metadata for a single node.
fetchNode :: Api -> DriveEndpoints -> DriveNodeId -> IO DriveNode
fetchNode api ep nid = fetchWith "fetchNode" api (nodeReq ep nid) parseNodeResponse


-- | Fetch the immediate children of a folder.
fetchChildren :: Api -> DriveEndpoints -> DriveNodeId -> IO [DriveNode]
fetchChildren api ep nid = fetchWith "fetchChildren" api (nodeReq ep nid) parseChildrenResponse


-- | Download the contents of a file node as a lazy 'LBS.ByteString'.
fetchFile :: Api -> DriveEndpoints -> FileData -> IO LBS.ByteString
fetchFile api ep fd
  | maybe True (== 0) (fdSize fd) = pure LBS.empty
  | otherwise = do
      url <- fetchWith "fetchFile (token)" api (downloadTokenReq (fdDocId fd) (fdZone fd) ep) parseDownloadUrl
      contentReq <- getReqFromUrl url
      contentResp <- rawRequest api contentReq
      checkStatus contentResp
      pure $ responseBody contentResp


-- | Create a new folder under the given parent node.
execCreateFolder :: Api -> DriveEndpoints -> DriveNodeId -> Text -> IO ()
execCreateFolder api ep parentId name = do
  resp <- rawRequest api req
  checkStatus resp
 where
  req =
    (createFolderReq ep)
      { requestBody = RequestBodyLBS (createFolderBody ep parentId name)
      , requestHeaders = (hContentType, "application/json") : requestHeaders (createFolderReq ep)
      }


-- | Rename a drive node (folder or file).
execRenameNode :: Api -> DriveEndpoints -> DriveNode -> Text -> IO ()
execRenameNode api ep node name = do
  resp <- rawRequest api req
  checkStatus resp
 where
  req =
    (renameNodeReq ep)
      { requestBody = RequestBodyLBS (renameNodeBody (nodeId node) (nodeEtag node) name)
      , requestHeaders = (hContentType, "application/json") : requestHeaders (renameNodeReq ep)
      }


-- | Move a drive node (folder or file) to the trash.
execDeleteNode :: Api -> DriveEndpoints -> DriveNode -> IO ()
execDeleteNode api ep node = do
  resp <- rawRequest api req
  checkStatus resp
 where
  req =
    (deleteNodeReq ep)
      { requestBody = RequestBodyLBS (deleteNodeBody ep (nodeId node) (nodeEtag node))
      , requestHeaders = (hContentType, "application/json") : requestHeaders (deleteNodeReq ep)
      }


{- | Upload file content into a folder using the 3-step iCloud Drive upload
protocol.
-}
execUploadFile :: Api -> DriveEndpoints -> FolderData -> Text -> LBS.ByteString -> IO ()
execUploadFile api ep folder filename content = do
  let zone = fnZone folder
      tokenBody = uploadTokenBodyBytes filename (LBS.length content)
      tokenReq' =
        (uploadTokenReq zone ep)
          { requestBody = RequestBodyLBS tokenBody
          , requestHeaders = (hContentType, "text/plain") : requestHeaders (uploadTokenReq zone ep)
          }
  (docId, uploadUrl) <- fetchWith "uploadFile (token)" api tokenReq' parseUploadTokenResponse
  uploadReq <- buildUploadReq filename content uploadUrl
  receipt <- fetchWith "uploadFile (content)" api uploadReq parseUploadReceiptResponse
  nowMs <- currentTimeMs
  let fDocId = folderDocId folder
      commitBody = buildCommitBody docId fDocId filename receipt nowMs
      commitReq' =
        (commitUploadReq zone ep)
          { requestBody = RequestBodyLBS commitBody
          , requestHeaders = (hContentType, "text/plain") : requestHeaders (commitUploadReq zone ep)
          }
  commitResp <- rawRequest api commitReq'
  checkStatus commitResp


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


nodeReq :: DriveEndpoints -> DriveNodeId -> Request
nodeReq ep nid =
  base
    { requestBody = RequestBodyLBS (nodeDetailsBody nid)
    , requestHeaders = (hContentType, "application/json") : requestHeaders base
    }
 where
  base = nodeDetailsReq ep


getReqFromUrl :: Text -> IO Request
getReqFromUrl = parseRequest . Text.unpack


fetchWith :: String -> Api -> Request -> (Value -> Parser a) -> IO a
fetchWith ctx api r parseF = do
  resp <- rawRequest api r
  checkStatus resp
  case eitherDecode (responseBody resp) of
    Left err -> throwIO (DriveParseError (ctx <> ": JSON decode error: " <> err))
    Right val -> either (throwIO . DriveParseError) pure $ parseEither parseF val


checkStatus :: Response a -> IO ()
checkStatus resp =
  let code = statusCode (responseStatus resp)
   in when (code >= 400) $ throwIO (DriveHttpError code)
