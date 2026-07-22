# hstratus-drive — unofficial access to iCloud Drive

`hstratus-drive` browses and downloads files from iCloud Drive using an
authenticated session from [`hstratus-auth`](../hstratus-auth/).

Provides access to the main CloudDocs tree: fetching the root folder,
listing folder contents, downloading files, and mutating the tree (create,
rename, delete, upload).


## Warning — use at your own risk

- This library is **unofficial** and not supported by Apple.
- The iCloud Drive API it uses is undocumented and may change without notice.


## Command-line tool

Run `hstratus-auth init` then `hstratus-auth login` first to store and authenticate
your credentials.

```
Usage: hstratus-drive COMMAND

  hstratus-drive: iCloud Drive access tool

Available options:
  -h,--help                Show this help text

Available commands:
  list-root                List immediate children of the top-level iCloud Drive
                           folder
  list-folder              List contents of a folder at a slash-separated path
                           from root
```

### `hstratus-drive list-root`

Lists the immediate children of the top-level iCloud Drive folder:

```
$ hstratus-drive list-root
FOLDER  Desktop
FOLDER  Documents
FOLDER  Photos
FILE    notes.txt  (1024 bytes)
```

```
Usage: hstratus-drive list-root [--china] [--log] [--log-file FILE] [--log-bodies]
                              [--redact]

  List immediate children of the top-level iCloud Drive folder

Available options:
  --china                  Use mainland China endpoints
  --log                    Append HTTP exchanges to the default log file
  --log-file FILE          Append HTTP exchanges to FILE
  --log-bodies             Include request bodies in the HTTP exchange log
  --redact                 Redact sensitive headers (tokens, cookies) in the log
  -h,--help                Show this help text
```

### `hstratus-drive list-folder`

Lists the contents of a folder by its slash-separated path from the Drive root:

```
$ hstratus-drive list-folder Documents/Work
FOLDER  Archive
FILE    report.pdf  (204800 bytes)
FILE    budget.xlsx  (38400 bytes)
```

```
Usage: hstratus-drive list-folder PATH [--china] [--log] [--log-file FILE]
                                [--log-bodies] [--redact]

  List contents of a folder at a slash-separated path from root

Available options:
  PATH                     Slash-separated path from root (e.g. Documents/Work)
  --china                  Use mainland China endpoints
  --log                    Append HTTP exchanges to the default log file
  --log-file FILE          Append HTTP exchanges to FILE
  --log-bodies             Include request bodies in the HTTP exchange log
  --redact                 Redact sensitive headers (tokens, cookies) in the log
  -h,--help                Show this help text
```


## Using the library

After a successful login with `hstratus-auth`, construct a `DriveApi` value and
use it to browse or download files.

### Browsing

```haskell
import Network.HStratus.Http (mkApi, login, AuthState (..))
import Network.HStratus.Http.Endpoints (Realm (..))
import Network.HStratus.Drive

example :: IO ()
example = do
  api <- mkApi Usual
  Authenticated sess ad <- login api
  da    <- mkDriveApi ad sess api
  root  <- driveRoot da
  nodes <- listFolder da (fnId root)
  mapM_ print nodes
```

### Downloading

```haskell
downloadExample :: DriveApi -> FileData -> IO ()
downloadExample da fd = do
  bytes <- downloadFile da fd
  -- bytes :: Data.ByteString.Lazy.ByteString
  print (Data.ByteString.Lazy.length bytes)
```

### Mutating

```haskell
mutationExample :: DriveApi -> FolderData -> IO ()
mutationExample da folder = do
  createFolder da (fnId folder) "New Folder"
  -- renameNode, deleteNode, and uploadFile follow the same pattern
```
