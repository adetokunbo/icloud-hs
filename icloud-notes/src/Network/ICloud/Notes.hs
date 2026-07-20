module Network.ICloud.Notes
  ( -- * Setup
    NotesEndpoints (..)
  , mkNotesEndpoints

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
  , module Network.ICloud.Notes.Note
  )
where

import Network.ICloud.Http (Api)
import Network.ICloud.Internal.Notes.Decode (decodeNoteBody)
import Network.ICloud.Internal.Notes.Download
  ( NotesError (..)
  , fetchFolders
  , fetchNote
  , fetchNotesInFolder
  , fetchRecent
  )
import Network.ICloud.Internal.Notes.Endpoints
  ( NotesEndpoints (..)
  , mkNotesEndpoints
  )
import Network.ICloud.Notes.Note


-- | Fetch recent notes, sorted by modification time descending.
recentNotes :: Api -> NotesEndpoints -> IO [NoteSummary]
recentNotes = fetchRecent


-- | Fetch all Notes folders.
noteFolders :: Api -> NotesEndpoints -> IO [NoteFolder]
noteFolders = fetchFolders


-- | Fetch notes belonging to the given folder.
notesInFolder :: Api -> NotesEndpoints -> FolderId -> IO [NoteSummary]
notesInFolder = fetchNotesInFolder


-- | Fetch a single note by ID. Returns 'Nothing' if the note has been deleted.
getNote :: Api -> NotesEndpoints -> NoteId -> IO (Maybe Note)
getNote = fetchNote
