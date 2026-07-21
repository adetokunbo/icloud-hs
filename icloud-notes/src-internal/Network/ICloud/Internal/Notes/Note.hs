{-# LANGUAGE StrictData #-}

module Network.ICloud.Internal.Notes.Note
  ( NoteId (..)
  , FolderId (..)
  , NoteSummary (..)
  , NoteFolder (..)
  , Note (..)
  , NoteText (..)
  , NoteRun (..)
  , NoteStyle (..)
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)


-- | CloudKit record name for a note (e.g. @\"Note\/ABCD-1234\"@).
newtype NoteId = NoteId
  { unNoteId :: Text
  -- ^ The raw CloudKit record name.
  }
  deriving (Eq, Ord, Show)


-- | CloudKit record name for a folder (e.g. @\"Folder\/ABCD-1234\"@).
newtype FolderId = FolderId
  { unFolderId :: Text
  -- ^ The raw CloudKit record name.
  }
  deriving (Eq, Ord, Show)


-- | Lightweight summary of a note returned by list and query operations.
data NoteSummary = NoteSummary
  { nsId :: NoteId
  -- ^ Stable CloudKit record identifier.
  , nsTitle :: Maybe Text
  -- ^ Decrypted title; 'Nothing' when the field is absent or unreadable.
  , nsSnippet :: Maybe Text
  -- ^ Decrypted snippet; 'Nothing' when absent or unreadable.
  , nsModified :: Maybe UTCTime
  -- ^ Last-modified timestamp from the @ModificationDate@ field.
  , nsFolderId :: Maybe FolderId
  -- ^ Containing folder; 'Nothing' for notes in the default folder.
  , nsDeleted :: Bool
  -- ^ 'True' when the note has been moved to the trash.
  , nsLocked :: Bool
  -- ^ 'True' when the record type is @PasswordProtectedNote@.
  }
  deriving (Eq, Ord, Show)


-- | A Notes folder returned by 'Network.ICloud.Notes.noteFolders'.
data NoteFolder = NoteFolder
  { nfId :: FolderId
  -- ^ Stable CloudKit record identifier.
  , nfName :: Maybe Text
  -- ^ Decrypted folder name; 'Nothing' when absent or unreadable.
  }
  deriving (Eq, Ord, Show)


-- | A full note including its raw (compressed protobuf) body bytes.
data Note = Note
  { noteInfo :: NoteSummary
  -- ^ Summary metadata for this note.
  , noteBodyBytes :: ByteString
  {- ^ Raw @TextDataEncrypted@ bytes (gzip-compressed protobuf).
  Pass to 'Network.ICloud.Notes.decodeNoteBody' to get 'NoteText'.
  -}
  }
  deriving (Eq, Ord, Show)


-- | Decoded plain-text content of a note with formatting runs.
data NoteText = NoteText
  { ntText :: Text
  -- ^ Full plain-text content of the note.
  , ntRuns :: [NoteRun]
  -- ^ Formatting runs parallel to 'ntText'.
  }
  deriving (Eq, Ord, Show)


-- | A single formatting run within a 'NoteText'.
data NoteRun = NoteRun
  { nrLength :: Int32
  -- ^ Number of characters this run covers in 'ntText'.
  , nrStyle :: Maybe NoteStyle
  -- ^ Paragraph style, if any.
  , nrBold :: Bool
  -- ^ 'True' when the run is bold.
  , nrItalic :: Bool
  -- ^ 'True' when the run is italic.
  , nrUnderline :: Bool
  -- ^ 'True' when the run is underlined.
  , nrLink :: Maybe Text
  -- ^ Hyperlink URL, if any.
  }
  deriving (Eq, Ord, Show)


-- | Paragraph style variants that can appear in a 'NoteRun'.
data NoteStyle
  = StyleTitle
  | StyleHeading
  | StyleSubheading
  | StyleMonospaced
  | StyleBullet
  | StyleDash
  | StyleNumbered
  | StyleChecklist
  deriving (Eq, Ord, Show)
