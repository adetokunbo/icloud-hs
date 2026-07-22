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

Run `hstratus-auth init` then `hstratus-auth login` first to store and authenticate
your credentials.

```
Usage: hstratus-notes COMMAND

  hstratus-notes: iCloud Notes access tool

Available options:
  -h,--help                Show this help text

Available commands:
  list-note-folders        List all iCloud Notes folders
  list-notes               List notes, optionally filtered by folder ID
```

### `hstratus-notes list-note-folders`

Lists all Notes folders, showing each folder's ID and name.

```
Usage: hstratus-notes list-note-folders
         [--china] [--log] [--log-file FILE] [--log-bodies] [--redact]

  List all iCloud Notes folders

Available options:
  --china                  Use mainland China endpoints
  --log                    Append HTTP exchanges to the default log file
  --log-file FILE          Append HTTP exchanges to FILE
  --log-bodies             Include request bodies in the HTTP exchange log
  --redact                 Redact sensitive headers (tokens, cookies) in the log
  -h,--help                Show this help text
```

### `hstratus-notes list-notes`

Lists notes sorted by modification time, optionally filtered to a single folder
by name.

```
Usage: hstratus-notes list-notes [--folder NAME] [--china] [--log]
                               [--log-file FILE] [--log-bodies] [--redact]

  List notes, optionally filtered by folder ID

Available options:
  --folder NAME            Folder name (e.g. TukTuk)
  --china                  Use mainland China endpoints
  --log                    Append HTTP exchanges to the default log file
  --log-file FILE          Append HTTP exchanges to FILE
  --log-bodies             Include request bodies in the HTTP exchange log
  --redact                 Redact sensitive headers (tokens, cookies) in the log
  -h,--help                Show this help text
```
