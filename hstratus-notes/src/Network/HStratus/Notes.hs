{-# LANGUAGE NamedFieldPuns #-}

module Network.HStratus.Notes
  ( -- * Setup
    NotesApi
  , mkNotesApi

    -- * Querying
  , recentNotes
  , noteFolders
  , notesInFolder
  , getNote

    -- * Decoding note bodies
  , decodeNoteBody

    -- * Errors
  , NotesError (..)

    -- * Re-exports
  , module Network.HStratus.Notes.Note
  )
where

import Network.HStratus.Http (Api)
import Network.HStratus.Internal.Notes.Decode (decodeNoteBody)
import Network.HStratus.Internal.Notes.Download
  ( NotesError (..)
  , fetchFolders
  , fetchNote
  , fetchNotesInFolder
  , fetchRecent
  )
import Network.HStratus.Internal.Notes.Endpoints
  ( NotesEndpoints
  , mkNotesEndpoints
  )
import Network.HStratus.Notes.Note
import Network.HStratus.Session (AccountData, Session)


{- | A bundled handle pairing a logged-in 'Api' with its notes endpoints.
Construct with 'mkNotesApi'; pass to all notes operations.
-}
data NotesApi = NotesApi
  { nApi :: !Api
  , nEp :: !NotesEndpoints
  }


-- | Pair a logged-in 'Api' with notes endpoints derived from its session data.
mkNotesApi :: AccountData -> Session -> Api -> IO NotesApi
mkNotesApi ad sess api = NotesApi api <$> mkNotesEndpoints ad sess


-- | Fetch recent notes, sorted by modification time descending.
recentNotes :: NotesApi -> IO [NoteSummary]
recentNotes NotesApi{nApi, nEp} = fetchRecent nApi nEp


-- | Fetch all Notes folders.
noteFolders :: NotesApi -> IO [NoteFolder]
noteFolders NotesApi{nApi, nEp} = fetchFolders nApi nEp


-- | Fetch notes belonging to the given folder.
notesInFolder :: NotesApi -> FolderId -> IO [NoteSummary]
notesInFolder NotesApi{nApi, nEp} = fetchNotesInFolder nApi nEp


-- | Fetch a single note by ID. Returns 'Nothing' if the note has been deleted.
getNote :: NotesApi -> NoteId -> IO (Maybe Note)
getNote NotesApi{nApi, nEp} = fetchNote nApi nEp
