{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.ICloud.Internal.Drive.Endpoints
  ( DriveEndpoints
  , CloudScope
  , mkDriveEndpoints
  , nodeDetailsReq
  , nodeDetailsBody
  , downloadTokenReq
  , createFolderReq
  , createFolderBody
  , renameNodeReq
  , renameNodeBody
  , deleteNodeReq
  , deleteNodeBody
  , uploadTokenReq
  , commitUploadReq
  )
where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
  ( Request (..)
  )
import Network.HTTP.Types (methodGet, methodPost, urlEncode)
import Network.ICloud.Http.Common
  ( icloudBrowserHeaders
  , lookupWebservice
  , stripTrailingSlash
  , withHeaders
  )
import Network.ICloud.Internal.Drive.Node (DriveNodeId (..))
import Network.ICloud.Session (AccountData (..), Session (..))


-- | Tag for the main CloudDocs tree; permits all drive operations.
data CloudScope


-- | Base requests and client ID needed to call the iCloud Drive API.
data DriveEndpoints s = DriveEndpoints
  { deServiceReq :: !Request
  -- ^ base request targeting the Drive service root (@drivews@)
  , deDocReq :: !Request
  -- ^ base request targeting the document root (@docws@)
  , deClientId :: !Text
  -- ^ client ID sent as a query parameter with every request
  }


{- | Construct 'DriveEndpoints' from the account data returned after login.

Fails if the @drivews@ or @docws@ service URLs are absent from the account
data.
-}
mkDriveEndpoints :: AccountData -> Session -> IO (DriveEndpoints CloudScope)
mkDriveEndpoints ad sess = do
  svcReq <- lookupWebservice "drivews" (adWebservices ad)
  docReq <- lookupWebservice "docws" (adWebservices ad)
  let deServiceReq = withHeaders icloudBrowserHeaders svcReq
      deDocReq = withHeaders icloudBrowserHeaders docReq
      deClientId = sessionClientId sess
  pure DriveEndpoints{deServiceReq, deDocReq, deClientId}


-- | Build the @POST retrieveItemDetailsInFolders@ request.
nodeDetailsReq :: DriveEndpoints s -> Request
nodeDetailsReq ep =
  withClientId ep $
    (deServiceReq ep)
      { path = stripTrailingSlash (path (deServiceReq ep)) <> "/retrieveItemDetailsInFolders"
      , method = methodPost
      }


-- | Build the JSON request body for @retrieveItemDetailsInFolders@.
nodeDetailsBody :: DriveNodeId -> LBS.ByteString
nodeDetailsBody (DriveNodeId nid) =
  "[{\"drivewsid\":\""
    <> LBS.fromStrict (encodeUtf8 nid)
    <> "\",\"partialData\":false}]"


-- | Build the @GET download/by_id@ request for a file in the given zone.
downloadTokenReq :: Text -> Text -> DriveEndpoints s -> Request
downloadTokenReq docId zone ep =
  (deDocReq ep)
    { path = stripTrailingSlash (path (deDocReq ep)) <> "/ws/" <> urlEncode False (encodeUtf8 zone) <> "/download/by_id"
    , method = methodGet
    , queryString =
        "clientId="
          <> urlEncode True (encodeUtf8 (deClientId ep))
          <> "&document_id="
          <> urlEncode True (encodeUtf8 docId)
    }


-- | Build the @POST createFolders@ request.
createFolderReq :: DriveEndpoints s -> Request
createFolderReq ep =
  withClientId ep $
    (deServiceReq ep)
      { path = stripTrailingSlash (path (deServiceReq ep)) <> "/createFolders"
      , method = methodPost
      }


-- | Build the JSON request body for @createFolders@.
createFolderBody :: DriveEndpoints s -> DriveNodeId -> Text -> LBS.ByteString
createFolderBody ep (DriveNodeId parentId) name =
  encode $
    object
      [ "destinationDrivewsId" .= parentId
      , "folders" .= [object ["clientId" .= deClientId ep, "name" .= name]]
      ]


-- | Build the @POST renameItems@ request.
renameNodeReq :: DriveEndpoints s -> Request
renameNodeReq ep =
  withClientId ep $
    (deServiceReq ep)
      { path = stripTrailingSlash (path (deServiceReq ep)) <> "/renameItems"
      , method = methodPost
      }


-- | Build the JSON request body for @renameItems@.
renameNodeBody :: DriveNodeId -> Text -> Text -> LBS.ByteString
renameNodeBody (DriveNodeId nid) etag name =
  encode $
    object
      ["items" .= [object ["drivewsid" .= nid, "etag" .= etag, "name" .= name]]]


-- | Build the @POST moveItemsToTrash@ request.
deleteNodeReq :: DriveEndpoints s -> Request
deleteNodeReq ep =
  withClientId ep $
    (deServiceReq ep)
      { path = stripTrailingSlash (path (deServiceReq ep)) <> "/moveItemsToTrash"
      , method = methodPost
      }


-- | Build the JSON request body for @moveItemsToTrash@.
deleteNodeBody :: DriveEndpoints s -> DriveNodeId -> Text -> LBS.ByteString
deleteNodeBody ep (DriveNodeId nid) etag =
  encode $
    object
      ["items" .= [object ["drivewsid" .= nid, "etag" .= etag, "clientId" .= deClientId ep]]]


-- | Build the @POST upload/web@ request for the given zone.
uploadTokenReq :: Text -> DriveEndpoints s -> Request
uploadTokenReq zone ep =
  withClientId ep $
    (deDocReq ep)
      { path =
          stripTrailingSlash (path (deDocReq ep))
            <> "/ws/"
            <> urlEncode False (encodeUtf8 zone)
            <> "/upload/web"
      , method = methodPost
      }


-- | Build the @POST update/documents@ commit request for the given zone.
commitUploadReq :: Text -> DriveEndpoints s -> Request
commitUploadReq zone ep =
  withClientId ep $
    (deDocReq ep)
      { path =
          stripTrailingSlash (path (deDocReq ep))
            <> "/ws/"
            <> urlEncode False (encodeUtf8 zone)
            <> "/update/documents"
      , method = methodPost
      }


withClientId :: DriveEndpoints s -> Request -> Request
withClientId ep req =
  let cid = "clientId=" <> urlEncode True (encodeUtf8 (deClientId ep))
      qs = queryString req
   in req{queryString = cid <> (if qs == "" then "" else "&" <> qs)}
