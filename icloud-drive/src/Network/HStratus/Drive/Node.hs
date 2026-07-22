{- |
Module      : Network.HStratus.Drive.Node
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Types representing nodes in the iCloud Drive file tree.

Every item in Drive is either a 'FolderData' or a 'FileData', wrapped in a
'DriveNode'. Folders are identified by a 'DriveNodeId'; files additionally
carry a document identifier used for download.

Use 'rootNodeId' to address the root of the main CloudDocs tree.
-}
module Network.HStratus.Drive.Node
  ( -- * Node sum type
    DriveNode (..)

    -- * Folder
  , FolderData (..)

    -- * File
  , FileData (..)
  , fileName

    -- * Identifiers
  , DriveNodeId (..)
  , rootNodeId
  )
where

import Network.HStratus.Internal.Drive.Node
  ( DriveNode (..)
  , DriveNodeId (..)
  , FileData (..)
  , FolderData (..)
  , fileName
  , rootNodeId
  )

