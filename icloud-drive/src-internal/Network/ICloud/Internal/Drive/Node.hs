{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.ICloud.Internal.Drive.Node
  ( -- * Node identifier
    DriveNodeId (..)
  , rootNodeId
  , appNodeId

    -- * App bundle identifier
  , BundleId (..)

    -- * Node types
  , DriveNode (..)
  , FolderData (..)
  , FileData (..)
  , fileName

    -- * App library
  , AppLibrary (..)
  , AppLibraryIcon (..)
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)


-- | Stable identifier for a node in iCloud Drive (the @drivewsid@ field).
newtype DriveNodeId = DriveNodeId {unDriveNodeId :: Text}
  deriving (Eq, Show)


-- | The node ID for the root of the main CloudDocs tree.
rootNodeId :: DriveNodeId
rootNodeId = DriveNodeId "FOLDER::com.apple.CloudDocs::root"


-- | The node ID for the documents folder of a specific app bundle.
appNodeId :: BundleId -> DriveNodeId
appNodeId (BundleId b) = DriveNodeId $ "FOLDER::" <> b <> "::documents"


-- | An Apple app bundle identifier (e.g. @com.apple.Pages@).
newtype BundleId = BundleId {unBundleId :: Text}
  deriving (Eq, Show)


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


-- | Metadata for an app library entry from @retrieveAppLibraries@.
data AppLibrary = AppLibrary
  { alBundleId :: !BundleId
  -- ^ the app's bundle identifier (use with 'appNodeId' or 'driveAppNode')
  , alName :: !(Maybe Text)
  -- ^ human-readable display name (e.g. @"Pages"@); absent for some apps
  , alDateCreated :: !UTCTime
  -- ^ when this library was registered with iCloud Drive
  , alIcons :: ![AppLibraryIcon]
  -- ^ app icon URLs at various sizes; empty when not provided
  }
  deriving (Eq, Show)


-- | A single icon entry within an 'AppLibrary'.
data AppLibraryIcon = AppLibraryIcon
  { aliUrl :: !Text
  -- ^ URL of the icon image
  , aliIconType :: !Text
  -- ^ platform tag (e.g. @"IOS"@, @"MAC"@)
  , aliSize :: !Int
  -- ^ icon dimension in points (e.g. @120@)
  }
  deriving (Eq, Show)
