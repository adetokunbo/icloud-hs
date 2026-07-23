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

The command-line interface is provided by the [`hstratus`](../hstratus/#readme)
package.  Use `hstratus drive list-root` and `hstratus drive list-folder` to
browse iCloud Drive.


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
