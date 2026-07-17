{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.ICloud.Internal.Drive.Endpoints
  ( DriveEndpoints
  , mkDriveEndpoints
  , nodeDetailsReq
  , nodeDetailsBody
  , appLibrariesReq
  , downloadTokenReq
  )
where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
  ( Request (..)
  , parseRequest
  )
import Network.HTTP.Types (methodGet, methodPost)
import Network.ICloud.Internal.Drive.Node (DriveNodeId (..))
import Network.ICloud.Session (AccountData (..), Session (..))


-- | Base requests and client ID needed to call the iCloud Drive API.
data DriveEndpoints = DriveEndpoints
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
mkDriveEndpoints :: AccountData -> Session -> IO DriveEndpoints
mkDriveEndpoints ad sess = do
  deServiceReq <- lookupAndParse "drivews" (adWebservices ad)
  deDocReq <- lookupAndParse "docws" (adWebservices ad)
  let deClientId = sessionClientId sess
  pure DriveEndpoints{deServiceReq, deDocReq, deClientId}


-- | Build the @POST retrieveItemDetailsInFolders@ request.
nodeDetailsReq :: DriveEndpoints -> Request
nodeDetailsReq ep =
  withClientId ep $
    (deServiceReq ep)
      { path = path (deServiceReq ep) <> "/retrieveItemDetailsInFolders"
      , method = methodPost
      }


-- | Build the JSON request body for @retrieveItemDetailsInFolders@.
nodeDetailsBody :: DriveNodeId -> LBS.ByteString
nodeDetailsBody (DriveNodeId nid) =
  "[{\"drivewsid\":\"" <> LBS.fromStrict (BS8.pack (Text.unpack nid)) <> "\",\"partialData\":false}]"


-- | Build the @GET retrieveAppLibraries@ request.
appLibrariesReq :: DriveEndpoints -> Request
appLibrariesReq ep =
  withClientId ep $
    (deServiceReq ep)
      { path = path (deServiceReq ep) <> "/retrieveAppLibraries"
      , method = methodGet
      }


-- | Build the @GET download/by_id@ request for a file in the given zone.
downloadTokenReq :: Text -> Text -> DriveEndpoints -> Request
downloadTokenReq docId zone ep =
  (deDocReq ep)
    { path = path (deDocReq ep) <> "/ws/" <> BS8.pack (Text.unpack zone) <> "/download/by_id"
    , method = methodGet
    , queryString =
        "clientId="
          <> BS8.pack (Text.unpack (deClientId ep))
          <> "&document_id="
          <> BS8.pack (Text.unpack docId)
    }


lookupAndParse :: Text -> Map Text Text -> IO Request
lookupAndParse key ws =
  case Map.lookup key ws of
    Nothing -> fail $ "icloud-drive: webservice URL not found for key: " <> Text.unpack key
    Just url -> parseRequest (Text.unpack url)


withClientId :: DriveEndpoints -> Request -> Request
withClientId ep req =
  req{queryString = "clientId=" <> BS8.pack (Text.unpack (deClientId ep))}
