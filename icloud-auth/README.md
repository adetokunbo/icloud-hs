# icloud-auth — unofficial authentication for iCloud services

`icloud-auth` authenticates with iCloud using Apple ID credentials stored on
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

```
Usage: icloud-auth COMMAND

  icloud-auth: iCloud authentication tool

Available options:
  -h,--help                Show this help text

Available commands:
  init                     Save Apple ID credentials to the config directory
  login                    Authenticate with iCloud
```

### Saving credentials

Run `icloud-auth init` once to save your Apple ID and password to
`$XDG_CONFIG_HOME/hs-icloud-auth/credentials.json`:

```
$ icloud-auth init
Apple ID: your-apple-id@example.com
Password:
Credentials saved.
```

### Authenticating

```
Usage: icloud-auth login [--china] [--log] [--log-file FILE] [--redact]

  Authenticate with iCloud

Available options:
  --china                  Use mainland China endpoints
  --log                    Append HTTP exchanges to the default log file
  --log-file FILE          Append HTTP exchanges to FILE
  --redact                 Redact sensitive headers (tokens, cookies) in the log
  -h,--help                Show this help text
```

Run `icloud-auth login` to authenticate using the saved credentials.  The full
sign-in flow runs interactively, prompting for a 2FA or 2SA verification code
when required:

```
$ icloud-auth login
Authenticated.
```

Use `--log` / `--log-file FILE` to record the HTTP exchange.  Add `--redact` to
scrub tokens and cookies from the log before writing.


## Using the library

The same two steps — saving credentials and authenticating — are available
programmatically.

### Saving credentials

Write a `credentials.json` file directly, or call `saveCredentials`:

```haskell
import Network.ICloud.Session (Credentials (..), saveCredentials)

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
import Network.ICloud.Http (mkApi, login, AuthState (..))
import Network.ICloud.Http.Endpoints (Realm (..))

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
import Network.ICloud.Http (loginWith)
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
