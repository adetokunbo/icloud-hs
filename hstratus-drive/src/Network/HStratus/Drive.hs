{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : Network.HStratus.Drive
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Access iCloud Drive using an authenticated session from @hstratus-auth@.

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
data DriveApi = DriveApi
  { dApi :: !Api
  , dEp :: !DriveEndpoints
  }


-- | Pair a logged-in 'Api' with drive endpoints derived from its session data.
mkDriveApi :: AccountData -> Session -> Api -> IO DriveApi
mkDriveApi ad sess api = DriveApi api <$> mkDriveEndpoints ad sess


-- | Fetch the root folder of the main CloudDocs tree.
driveRoot :: DriveApi -> IO FolderData
driveRoot DriveApi{dApi, dEp} = do
  node <- fetchNode dApi dEp rootNodeId
  case node of
    DriveFolder fd -> pure fd
    DriveFile _ -> throwIO DriveInvalidRoot


-- | Fetch the immediate children of a folder.
listFolder :: DriveApi -> DriveNodeId -> IO [DriveNode]
listFolder DriveApi{dApi, dEp} = fetchChildren dApi dEp


-- | Download the contents of a file as a lazy 'LBS.ByteString'.
downloadFile :: DriveApi -> FileData -> IO LBS.ByteString
downloadFile DriveApi{dApi, dEp} = fetchFile dApi dEp


-- | Create a new folder inside an existing folder.
createFolder :: DriveApi -> DriveNodeId -> Text -> IO ()
createFolder DriveApi{dApi, dEp} = execCreateFolder dApi dEp


-- | Rename a node (folder or file) to a new name.
renameNode :: DriveApi -> DriveNode -> Text -> IO ()
renameNode DriveApi{dApi, dEp} = execRenameNode dApi dEp


-- | Move a node (folder or file) to the trash.
deleteNode :: DriveApi -> DriveNode -> IO ()
deleteNode DriveApi{dApi, dEp} = execDeleteNode dApi dEp


-- | Upload a file into a folder.
uploadFile :: DriveApi -> FolderData -> Text -> LBS.ByteString -> IO ()
uploadFile DriveApi{dApi, dEp} = execUploadFile dApi dEp
