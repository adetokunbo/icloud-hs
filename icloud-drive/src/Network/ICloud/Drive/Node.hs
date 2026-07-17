{- |
Module      : Network.ICloud.Drive.Node
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Types representing nodes in the iCloud Drive file tree.

Every item in Drive is either a 'FolderData' or a 'FileData', wrapped in a
'DriveNode'. Folders are identified by a 'DriveNodeId'; files additionally
carry a document identifier used for download.

Use 'rootNodeId' to address the root of the main CloudDocs tree, or
'appNodeId' to address the documents folder of a specific app by its
'BundleId'.
-}
module Network.ICloud.Drive.Node
  ( -- * Node sum type
    DriveNode (..)

    -- * Folder
  , FolderData (..)

    -- * File
  , FileData (..)
  , fileName

    -- * Node helpers
  , nodeId
  , nodeEtag

    -- * Identifiers
  , DriveNodeId (..)
  , rootNodeId
  , appNodeId
  , BundleId (..)

    -- * App library
  , AppLibrary (..)
  , AppLibraryIcon (..)
  )
where

import Network.ICloud.Internal.Drive.Node
  ( AppLibrary (..)
  , AppLibraryIcon (..)
  , BundleId (..)
  , DriveNode (..)
  , DriveNodeId (..)
  , FileData (..)
  , FolderData (..)
  , appNodeId
  , fileName
  , nodeEtag
  , nodeId
  , rootNodeId
  )

