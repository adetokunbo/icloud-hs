{-# LANGUAGE StrictData #-}

module Network.ICloud.Internal.Notes.Note
  ( NoteId (..)
  , FolderId (..)
  , NoteSummary (..)
  , NoteFolder (..)
  , Note (..)
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)


newtype NoteId = NoteId {unNoteId :: Text}
  deriving (Eq, Ord, Show)


newtype FolderId = FolderId {unFolderId :: Text}
  deriving (Eq, Ord, Show)


data NoteSummary = NoteSummary
  { nsId :: NoteId
  , nsTitle :: Maybe Text
  , nsSnippet :: Maybe Text
  , nsModified :: Maybe UTCTime
  , nsFolderId :: Maybe FolderId
  , nsDeleted :: Bool
  , nsLocked :: Bool
  }
  deriving (Eq, Show)


data NoteFolder = NoteFolder
  { nfId :: FolderId
  , nfName :: Maybe Text
  }
  deriving (Eq, Show)


data Note = Note
  { noteInfo :: NoteSummary
  , noteBodyBytes :: ByteString
  }
  deriving (Eq, Show)
