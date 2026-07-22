{- |
Module      : Network.HStratus.Drive
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Access iCloud Drive using an authenticated session from @icloud-auth@.

After a successful login with 'Network.HStratus.Http.login', construct a
'DriveApi' value from the returned 'AccountData', 'Session', and 'Api'
handle, then use it to browse and download files.

@
import Network.HStratus.Http   (login, mkApi)
import Network.HStratus.Http.Endpoints (Realm (..))
import Network.HStratus.Drive

main :: IO ()
main = do
  api <- mkApi Usual
  Authenticated sess ad <- login api
  da  <- mkDriveApi ad sess api
  root <- driveRoot da
  nodes <- listFolder da (fnId root)
  print nodes
@
-}
module Network.HStratus.Drive
  ( -- * Setup
    DriveApi
  , mkDriveApi

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

    -- * Errors
  , DriveError (..)

    -- * Re-exports
  , module Network.HStratus.Drive.Node
  )
where

import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Network.HStratus.Drive.Node
import Network.HStratus.Http (Api)
import Network.HStratus.Internal.Drive.Download
  ( DriveError (..)
  , execCreateFolder
  , execDeleteNode
  , execRenameNode
  , execUploadFile
  , fetchChildren
  , fetchFile
  , fetchNode
  )
import Network.HStratus.Internal.Drive.Endpoints
  ( DriveEndpoints
  , mkDriveEndpoints
  )
import Network.HStratus.Session (AccountData, Session)


{- | A bundled handle pairing a logged-in 'Api' with its drive endpoints.
Construct with 'mkDriveApi'; pass to all drive operations.
-}
data DriveApi = DriveApi !Api !DriveEndpoints


-- | Pair a logged-in 'Api' with drive endpoints derived from its session data.
mkDriveApi :: AccountData -> Session -> Api -> IO DriveApi
mkDriveApi ad sess api = DriveApi api <$> mkDriveEndpoints ad sess


-- | Fetch the root folder of the main CloudDocs tree.
driveRoot :: DriveApi -> IO FolderData
driveRoot (DriveApi api ep) = do
  node <- fetchNode api ep rootNodeId
  case node of
    DriveFolder fd -> pure fd
    DriveFile _ -> throwIO DriveInvalidRoot


-- | Fetch the immediate children of a folder.
listFolder :: DriveApi -> DriveNodeId -> IO [DriveNode]
listFolder (DriveApi api ep) = fetchChildren api ep


-- | Download the contents of a file as a lazy 'LBS.ByteString'.
downloadFile :: DriveApi -> FileData -> IO LBS.ByteString
downloadFile (DriveApi api ep) = fetchFile api ep


-- | Create a new folder inside an existing folder.
createFolder :: DriveApi -> DriveNodeId -> Text -> IO ()
createFolder (DriveApi api ep) = execCreateFolder api ep


-- | Rename a node (folder or file) to a new name.
renameNode :: DriveApi -> DriveNode -> Text -> IO ()
renameNode (DriveApi api ep) = execRenameNode api ep


-- | Move a node (folder or file) to the trash.
deleteNode :: DriveApi -> DriveNode -> IO ()
deleteNode (DriveApi api ep) = execDeleteNode api ep


-- | Upload a file into a folder.
uploadFile :: DriveApi -> FolderData -> Text -> LBS.ByteString -> IO ()
uploadFile (DriveApi api ep) = execUploadFile api ep
