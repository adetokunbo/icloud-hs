{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.HStratus.Internal.Drive.Node
  ( -- * Node identifier
    DriveNodeId (..)
  , rootNodeId

    -- * Node types
  , DriveNode (..)
  , FolderData (..)
  , FileData (..)
  , fileName
  , nodeId
  , nodeEtag
  , folderDocId
  )
where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)


-- | Stable identifier for a node in iCloud Drive (the @drivewsid@ field).
newtype DriveNodeId = DriveNodeId {unDriveNodeId :: Text}
  deriving (Eq, Show)


-- | The node ID for the root of the main CloudDocs tree.
rootNodeId :: DriveNodeId
rootNodeId = DriveNodeId "FOLDER::com.apple.CloudDocs::root"


-- | A node in the iCloud Drive tree — either a folder or a file.
data DriveNode
  = DriveFolder FolderData
  | DriveFile FileData
  deriving (Eq, Show)


-- | Metadata for a folder node.
data FolderData = FolderData
  { fnId :: !DriveNodeId
  -- ^ stable node identifier (@drivewsid@)
  , fnEtag :: !Text
  -- ^ version tag; required for rename and delete
  , fnName :: !Text
  -- ^ display name of the folder
  , fnZone :: !Text
  -- ^ CloudDocs zone (e.g. @com.apple.CloudDocs@)
  , fnDateCreated :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)


-- | Metadata for a file node.
data FileData = FileData
  { fdId :: !DriveNodeId
  -- ^ stable node identifier (@drivewsid@)
  , fdDocId :: !Text
  -- ^ document identifier (@docwsid@); used for download and upload
  , fdEtag :: !Text
  -- ^ version tag; required for rename and delete
  , fdName :: !Text
  -- ^ base file name (without extension)
  , fdExtension :: !(Maybe Text)
  -- ^ file extension, if present
  , fdZone :: !Text
  -- ^ CloudDocs zone (e.g. @com.apple.CloudDocs@)
  , fdSize :: !(Maybe Int64)
  -- ^ file size in bytes; @Nothing@ for zero-byte files
  , fdDateCreated :: !(Maybe UTCTime)
  , fdDateModified :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)


-- | The full display name of a file, with extension appended if present.
fileName :: FileData -> Text
fileName fd = case fdExtension fd of
  Nothing -> fdName fd
  Just ext -> fdName fd <> Text.pack "." <> ext


-- | Extract the stable identifier from any node.
nodeId :: DriveNode -> DriveNodeId
nodeId (DriveFolder fd) = fnId fd
nodeId (DriveFile fd) = fdId fd


-- | Extract the version tag from any node.
nodeEtag :: DriveNode -> Text
nodeEtag (DriveFolder fd) = fnEtag fd
nodeEtag (DriveFile fd) = fdEtag fd


-- | Derive the @docwsid@ of a folder from its 'DriveNodeId' and zone.
folderDocId :: FolderData -> Text
folderDocId fd =
  let DriveNodeId nid = fnId fd
      prefix = "FOLDER::" <> fnZone fd <> "::"
   in fromMaybe nid (Text.stripPrefix prefix nid)
