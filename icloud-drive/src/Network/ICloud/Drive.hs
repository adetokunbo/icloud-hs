{- |
Module      : Network.ICloud.Drive
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Access iCloud Drive using an authenticated session from @icloud-auth@.

After a successful login with 'Network.ICloud.Http.login', construct a
'DriveEndpoints' value from the returned 'AccountData' and 'Session', then
use it together with the 'Api' handle to browse and download files.

@
import Network.ICloud.Http   (login, mkApi)
import Network.ICloud.Drive

main :: IO ()
main = do
  api <- mkApi GlobalRealm
  Authenticated sess ad <- login api
  ep  <- mkDriveEndpoints ad sess
  root <- driveRoot api ep
  nodes <- listFolder api ep (fnId root)
  print nodes
@
-}
module Network.ICloud.Drive
  ( -- * Setup
    DriveEndpoints
  , mkDriveEndpoints

    -- * Browsing
  , driveRoot
  , listFolder
  , driveAppNode

    -- * Downloading
  , downloadFile

    -- * Discovery
  , listAppLibrariesRaw

    -- * Re-exports
  , module Network.ICloud.Drive.Node
  )
where

import qualified Data.ByteString.Lazy as LBS
import Network.ICloud.Drive.Node
import Network.ICloud.Http (Api)
import Network.ICloud.Internal.Drive.Download
  ( fetchAppLibrariesRaw
  , fetchChildren
  , fetchFile
  , fetchNode
  )
import Network.ICloud.Internal.Drive.Endpoints
  ( DriveEndpoints
  , mkDriveEndpoints
  )


-- | Fetch the root folder of the main CloudDocs tree.
driveRoot :: Api -> DriveEndpoints -> IO FolderData
driveRoot api ep = do
  node <- fetchNode api ep rootNodeId
  case node of
    DriveFolder fd -> pure fd
    DriveFile _ -> fail "driveRoot: unexpected file node at root"


-- | Fetch the immediate children of a folder.
listFolder :: Api -> DriveEndpoints -> DriveNodeId -> IO [DriveNode]
listFolder = fetchChildren


-- | Fetch the documents folder for a specific app bundle.
driveAppNode :: Api -> DriveEndpoints -> BundleId -> IO FolderData
driveAppNode api ep bid = do
  node <- fetchNode api ep (appNodeId bid)
  case node of
    DriveFolder fd -> pure fd
    DriveFile _ -> fail "driveAppNode: unexpected file node"


-- | Download the contents of a file as a lazy 'LBS.ByteString'.
downloadFile :: Api -> DriveEndpoints -> FileData -> IO LBS.ByteString
downloadFile = fetchFile


{- | Fetch the raw JSON body from @GET retrieveAppLibraries@.

Use this to inspect the live response shape before implementing 'listAppLibraries'.
-}
listAppLibrariesRaw :: Api -> DriveEndpoints -> IO LBS.ByteString
listAppLibrariesRaw = fetchAppLibrariesRaw
