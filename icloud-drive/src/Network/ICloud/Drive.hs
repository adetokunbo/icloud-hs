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

    -- * App folders
  , driveAppNodeById

    -- * Mutations
  , createFolder
  , renameNode
  , deleteNode
  , uploadFile

    -- * Discovery
  , listAppLibraries
  , listAppLibrariesRaw

    -- * Re-exports
  , module Network.ICloud.Drive.Node
  )
where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Network.ICloud.Drive.Node
import Network.ICloud.Http (Api)
import Network.ICloud.Internal.Drive.Download
  ( execAppNode
  , execCreateFolder
  , execDeleteNode
  , execRenameNode
  , execUploadFile
  , fetchAppLibraries
  , fetchAppLibrariesRaw
  , fetchChildren
  , fetchFile
  , fetchNode
  )
import Network.ICloud.Internal.Drive.Endpoints
  ( DriveEndpoints
  , mkDriveEndpoints
  , toAppScope
  )


-- | Fetch the root folder of the main CloudDocs tree.
driveRoot :: Api -> DriveEndpoints CloudScope -> IO FolderData
driveRoot api ep = do
  node <- fetchNode api ep rootNodeId
  case node of
    DriveFolder fd -> pure fd
    DriveFile _ -> fail "driveRoot: unexpected file node at root"


-- | Fetch the immediate children of a folder.
listFolder :: Api -> DriveEndpoints s -> DriveNodeId -> IO [DriveNode]
listFolder = fetchChildren


{- | Fetch an app's document folder by its stable node identifier.

The 'DriveNodeId' should come from 'alNodeId' on an 'AppLibrary' returned by
'listAppLibraries'.  The returned 'AppScope' endpoint permits only read
operations ('listFolder', 'downloadFile').
-}
driveAppNodeById :: Api -> DriveEndpoints CloudScope -> DriveNodeId -> IO (DriveEndpoints AppScope, FolderData)
driveAppNodeById = execAppNode


{- | Fetch the documents folder for a specific app bundle.

Returns an 'AppScope' endpoint together with the folder.  The 'AppScope'
endpoint may only be used for read operations ('listFolder', 'downloadFile');
passing it to any mutation raises a compile error.
-}
driveAppNode :: Api -> DriveEndpoints CloudScope -> BundleId -> IO (DriveEndpoints AppScope, FolderData)
driveAppNode api ep bid = do
  node <- fetchNode api ep (appNodeId bid)
  case node of
    DriveFolder fd -> pure (toAppScope ep, fd)
    DriveFile _ -> fail "driveAppNode: unexpected file node"


-- | Download the contents of a file as a lazy 'LBS.ByteString'.
downloadFile :: Api -> DriveEndpoints s -> FileData -> IO LBS.ByteString
downloadFile = fetchFile


-- | Create a new folder inside an existing folder.
createFolder :: Api -> DriveEndpoints CloudScope -> DriveNodeId -> Text -> IO ()
createFolder = execCreateFolder


-- | Rename a node (folder or file) to a new name.
renameNode :: Api -> DriveEndpoints CloudScope -> DriveNode -> Text -> IO ()
renameNode = execRenameNode


-- | Move a node (folder or file) to the trash.
deleteNode :: Api -> DriveEndpoints CloudScope -> DriveNode -> IO ()
deleteNode = execDeleteNode


-- | List the app libraries registered with this account's iCloud Drive.
listAppLibraries :: Api -> DriveEndpoints s -> IO [AppLibrary]
listAppLibraries = fetchAppLibraries


-- | Fetch the raw JSON body from @GET retrieveAppLibraries@.
listAppLibrariesRaw :: Api -> DriveEndpoints s -> IO LBS.ByteString
listAppLibrariesRaw = fetchAppLibrariesRaw


-- | Upload a file into a folder.
uploadFile :: Api -> DriveEndpoints CloudScope -> FolderData -> Text -> LBS.ByteString -> IO ()
uploadFile = execUploadFile
