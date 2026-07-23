# hstratus-notes — unofficial access to iCloud Notes

`hstratus-notes` reads notes and folders from iCloud Notes using an authenticated
session from [`hstratus-auth`](../hstratus-auth/).

Provides read-only access to the Notes CloudKit database: listing folders,
fetching recent notes, and downloading note content.


## Warning — use at your own risk

- This library is **unofficial** and not supported by Apple.
- The iCloud Notes API it uses is undocumented and may change without notice.


## Usage

After a successful login with `hstratus-auth`, construct `NotesEndpoints` and use
them to browse notes.

```haskell
import Network.HStratus.Http (mkApi, login, AuthState (..))
import Network.HStratus.Http.Endpoints (Realm (..))
import Network.HStratus.Notes

example :: IO ()
example = do
  api <- mkApi Usual
  Authenticated sess ad <- login api
  ep    <- mkNotesEndpoints ad sess
  notes <- recentNotes api ep
  mapM_ print notes
```


## Command-line tool

The command-line interface is provided by the [`hstratus`](../hstratus/#readme)
package.  Use `hstratus notes list-note-folders` and `hstratus notes list-notes`
to browse iCloud Notes.
