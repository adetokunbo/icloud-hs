# hstratus-auth — unofficial authentication for iCloud services

`hstratus-auth` authenticates with iCloud using Apple ID credentials stored on
disk.  The full sign-in flow — SRP credential exchange followed by any required
two-factor (2FA) or legacy two-step (2SA) challenge — runs automatically,
prompting the terminal for verification codes when needed.  On success it caches
a session token for use with other iCloud services.


## Warning — use at your own risk

- This library is **unofficial** and not supported by Apple.
- The iCloud authentication protocol it uses is undocumented and may change
  without notice.


## Reauthentication

The session token expires after a period set by iCloud (approximately two months
at the time of writing).  When it does, authenticate again to refresh it.


## Command-line tool

The command-line interface is provided by the [`hstratus`](../hstratus/#readme)
package.  Use `hstratus auth init` and `hstratus auth login` to save credentials
and authenticate.


## Using the library

The same two steps — saving credentials and authenticating — are available
programmatically.

### Saving credentials

Write a `credentials.json` file directly, or call `saveCredentials`:

```haskell
import Network.HStratus.Session (Credentials (..), saveCredentials)

saveCreds :: IO ()
saveCreds =
  saveCredentials $ Credentials
    { accountName = "your-apple-id@example.com"
    , password    = "your-password"
    }
```

### Authenticating

Create an `Api` handle with `mkApi`, then call `login`:

```haskell
import Network.HStratus.Http (mkApi, login, AuthState (..))
import Network.HStratus.Http.Endpoints (Realm (..))

example :: IO ()
example = do
  api <- mkApi Usual  -- or China for mainland China accounts
  result <- login api
  case result of
    Authenticated _session _accountData -> putStrLn "Authenticated!"
    _                                   -> putStrLn "Unexpected result"
```

### Injectable callbacks

Pass your own callbacks to `loginWith` to replace the interactive prompts —
useful in automation or tests.  The snippet below shows the code-reader; the
phone-selector and device-selector arguments follow the same pattern.

```haskell
import Network.HStratus.Http (loginWith)
import qualified Data.Text.IO as Text

exampleWith :: Api -> IO AuthState
exampleWith api = loginWith readCode (\_ -> pure Nothing) chooseDevice api
 where
  readCode codeLen = do
    Text.putStrLn $ "Enter the " <> Text.pack (show codeLen) <> "-digit code:"
    Text.getLine
  chooseDevice (d:_) = pure d
  chooseDevice []    = ioError (userError "no 2SA devices available")
```

If you already hold a `Requires2FA` or `Requires2SA` value from a prior call,
resume with `completeTwoFactor` / `completeTwoFactorWith` or `complete2SA` /
`complete2SAWith`.
