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
  )
where

import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
  ( Request
  , RequestBody (..)
  , Response (..)
  , parseRequest
  , requestBody
  , requestHeaders
  )
import Network.HTTP.Types (hContentType, statusCode)
import Network.ICloud.Http (Api, rawRequest)
import Network.ICloud.Internal.Drive.Endpoints
  ( DriveEndpoints
  , appLibrariesReq
  , createFolderBody
  , createFolderReq
  , deleteNodeBody
  , deleteNodeReq
  , downloadTokenReq
  , nodeDetailsBody
  , nodeDetailsReq
  , renameNodeBody
  , renameNodeReq
  )
import Network.ICloud.Internal.Drive.Node
  ( AppLibrary
  , DriveNode
  , DriveNodeId
  , FileData (..)
  , nodeEtag
  , nodeId
  )
import Network.ICloud.Internal.Drive.NodeData
  ( parseAppLibrariesResponse
  , parseChildrenResponse
  , parseDownloadUrl
  , parseNodeResponse
  )


-- | Fetch metadata for a single node.
fetchNode :: Api -> DriveEndpoints -> DriveNodeId -> IO DriveNode
fetchNode api ep nid = do
  resp <- rawRequest api (nodeReq ep nid)
  checkStatus "fetchNode" resp
  body <- decodeBody "fetchNode" resp
  either fail pure $ parseEither parseNodeResponse body


-- | Fetch the immediate children of a folder.
fetchChildren :: Api -> DriveEndpoints -> DriveNodeId -> IO [DriveNode]
fetchChildren api ep nid = do
  resp <- rawRequest api (nodeReq ep nid)
  checkStatus "fetchChildren" resp
  body <- decodeBody "fetchChildren" resp
  either fail pure $ parseEither parseChildrenResponse body


-- | Download the contents of a file node as a lazy 'LBS.ByteString'.
fetchFile :: Api -> DriveEndpoints -> FileData -> IO LBS.ByteString
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
fetchAppLibraries :: Api -> DriveEndpoints -> IO [AppLibrary]
fetchAppLibraries api ep = do
  resp <- rawRequest api (appLibrariesReq ep)
  checkStatus "fetchAppLibraries" resp
  body <- decodeBody "fetchAppLibraries" resp
  either fail pure $ parseEither parseAppLibrariesResponse body


-- | Fetch the raw JSON body from @GET retrieveAppLibraries@.
fetchAppLibrariesRaw :: Api -> DriveEndpoints -> IO LBS.ByteString
fetchAppLibrariesRaw api ep = do
  resp <- rawRequest api (appLibrariesReq ep)
  checkStatus "fetchAppLibrariesRaw" resp
  pure $ responseBody resp


-- | Create a new folder under the given parent node.
execCreateFolder :: Api -> DriveEndpoints -> DriveNodeId -> Text -> IO ()
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
execRenameNode :: Api -> DriveEndpoints -> DriveNode -> Text -> IO ()
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
execDeleteNode :: Api -> DriveEndpoints -> DriveNode -> IO ()
execDeleteNode api ep node = do
  resp <- rawRequest api req
  checkStatus "deleteNode" resp
 where
  req =
    (deleteNodeReq ep)
      { requestBody = RequestBodyLBS (deleteNodeBody ep (nodeId node) (nodeEtag node))
      , requestHeaders = (hContentType, "application/json") : requestHeaders (deleteNodeReq ep)
      }


nodeReq :: DriveEndpoints -> DriveNodeId -> Request
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
