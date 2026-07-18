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

    -- * Downloading
  , downloadFile

    -- * Mutations
  , createFolder
  , renameNode
  , deleteNode
  , uploadFile

    -- * Re-exports
  , module Network.ICloud.Drive.Node
  )
where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Network.ICloud.Drive.Node
import Network.ICloud.Http (Api)
import Network.ICloud.Internal.Drive.Download
  ( execCreateFolder
  , execDeleteNode
  , execRenameNode
  , execUploadFile
  , fetchChildren
  , fetchFile
  , fetchNode
  )
import Network.ICloud.Internal.Drive.Endpoints
  ( DriveEndpoints
  , mkDriveEndpoints
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


-- | Upload a file into a folder.
uploadFile :: Api -> DriveEndpoints CloudScope -> FolderData -> Text -> LBS.ByteString -> IO ()
uploadFile = execUploadFile
