# icloud-drive — unofficial access to iCloud Drive

`icloud-drive` browses and downloads files from iCloud Drive using an
authenticated session from [`icloud-auth`](../icloud-auth/).

Provides access to the main CloudDocs tree: fetching the root folder,
listing folder contents, downloading files, and mutating the tree (create,
rename, delete, upload).


## Warning — use at your own risk

- This library is **unofficial** and not supported by Apple.
- The iCloud Drive API it uses is undocumented and may change without notice.


## Usage

After a successful login with `icloud-auth`, construct a `DriveApi` value and
use it to browse or download files.

```haskell
import Network.ICloud.Http (mkApi, login, AuthState (..))
import Network.ICloud.Http.Endpoints (Realm (..))
import Network.ICloud.Drive

example :: IO ()
example = do
  api <- mkApi Usual
  Authenticated sess ad <- login api
  da    <- mkDriveApi ad sess api
  root  <- driveRoot da
  nodes <- listFolder da (fnId root)
  mapM_ print nodes
```


## Command-line tool

Run `icloud-auth init` then `icloud-auth` first to store and authenticate your
credentials.

```
Usage: icloud-drive COMMAND

  icloud-drive: iCloud Drive access tool

Available options:
  -h,--help                Show this help text

Available commands:
  list-root                List immediate children of the top-level iCloud Drive
                           folder
  list-folder              List contents of a folder at a slash-separated path
                           from root
```

### `icloud-drive list-root`

Lists the immediate children of the top-level iCloud Drive folder.

```
Usage: icloud-drive list-root [--china] [--log] [--log-file FILE] [--log-bodies]
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

### `icloud-drive list-folder`

Lists the contents of a folder by its slash-separated path from the Drive root.

```
Usage: icloud-drive list-folder PATH [--china] [--log] [--log-file FILE]
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
