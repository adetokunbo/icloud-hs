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


data NoteText = NoteText
  { ntText :: Text
  , ntRuns :: [NoteRun]
  }
  deriving (Eq, Show)


data NoteRun = NoteRun
  { nrLength :: Int32
  , nrStyle :: Maybe NoteStyle
  , nrBold :: Bool
  , nrItalic :: Bool
  , nrUnderline :: Bool
  , nrLink :: Maybe Text
  }
  deriving (Eq, Show)


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
