# icloud-auth — unofficial authentication for iCloud services

`icloud-auth` authenticates with iCloud using Apple ID credentials stored on
disk.  The full sign-in flow — SRP credential exchange followed by any required
two-factor (2FA) or legacy two-step (2SA) challenge — is handled by a single
`login` call.  On success the library caches a session token for use with other
iCloud services.


## Warning — use at your own risk

- This library is **unofficial** and not supported by Apple.
- The iCloud authentication protocol it uses is undocumented and may change
  without notice.


## Reauthentication

The session token expires after a period set by iCloud (approximately two months
at the time of writing).  When it does, call `login` again to refresh it.


## Usage

### Store your credentials

Save your Apple ID and password to
`$XDG_CONFIG_HOME/hs-icloud-auth/credentials.json`:

```bash
ICLOUD_CONF="${XDG_CONFIG_HOME:=${HOME}/.config}/hs-icloud-auth"
mkdir -p "$ICLOUD_CONF"
cat << EOF > "$ICLOUD_CONF/credentials.json"
{
  "accountName": "your-apple-id@example.com",
  "password":    "your-password"
}
EOF
chmod 600 "$ICLOUD_CONF/credentials.json"
```

### Typical usage

Create an `Api` handle with `mkApi`, then call `login`.  The full sign-in flow
— SRP credential exchange, any 2FA or 2SA challenge, and the final account-login
request — runs automatically, prompting the terminal for verification codes when
needed.

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


## Command-line tool

```
Usage: icloud-auth [COMMAND | [--china] [--log] [--log-file FILE] [--redact]]

  icloud-auth: iCloud authentication tool

Available options:
  --china                  Use mainland China endpoints
  --log                    Append HTTP exchanges to the default log file
  --log-file FILE          Append HTTP exchanges to FILE
  --redact                 Redact sensitive headers (tokens, cookies) in the log
  -h,--help                Show this help text

Available commands:
  init                     Save Apple ID credentials to the config directory
```

### `icloud-auth init`

Prompts for your Apple ID and password and saves them to
`$XDG_CONFIG_HOME/hs-icloud-auth/credentials.json`.

### Authenticating

Running `icloud-auth` without a subcommand performs authentication using the
saved credentials.  The full sign-in flow runs interactively, prompting for a
2FA or 2SA code when required.  Use `--log` / `--log-file` to record the HTTP
exchange; `--redact` scrubs tokens and cookies from the log.
