# icloud-auth - unofficial auth for iCloud services

[![GitHub CI](https://github.com/adetokunbo/icloud-hs/actions/workflows/cabal.yml/badge.svg)](https://github.com/adetokunbo/icloud-hs/actions)
[![Stackage Nightly](http://stackage.org/package/icloud-auth/badge/nightly)](http://stackage.org/nightly/package/icloud-auth)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/icloud-hs/blob/master/LICENSE)

`icloud-auth` allows logon to [iCloud] servers; upon provision of the username and
password, it retrieves and stores an authorization token, for use in programmatic
sessions with other iCloud services.


## Warning - use at your own risk

- It is unofficial, *not* supported by Apple, and not guaranteed to work.

- The iCloud service APIs it uses are not officially documented and can be
  changed at any time. Use at your own risk; in the worst case, Apple might ban
  your account!


## Reauthentication

The retrieved authorization token expires, after which re-authentication is
required. The expiry duration is set by iCloud; at the time of writing it is
approximately two months.


## Usage

### Store your credentials

Save your Apple ID and password to `$XDG_CONFIG_HOME/hs-icloud-auth/credential.json`:

```bash
ICLOUD_AUTH_CONF="${XDG_CONFIG_HOME:=${HOME}/.config}/hs-icloud-auth"
mkdir -p "$ICLOUD_AUTH_CONF"
cat << EOF > "$ICLOUD_AUTH_CONF/credential.json"
{
  "accountName": "your-apple-id@example.com",
  "password":    "your-password"
}
EOF
chmod 600 "$ICLOUD_AUTH_CONF/credential.json"
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
  api <- mkApi GlobalRealm  -- or ChinaRealm for mainland China accounts
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
exampleWith api = loginWith readCode (\_ -> pure Nothing) (\ds -> pure (head ds)) api
 where
  readCode codeLen = do
    Text.putStrLn $ "Enter the " <> Text.pack (show codeLen) <> "-digit verification code:"
    Text.getLine
```

If you already hold a `Requires2FA` or `Requires2SA` value from a prior call,
resume with `completeTwoFactor` / `completeTwoFactorWith` or `complete2SA` /
`complete2SAWith`.


[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/icloud-auth.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=icloud-auth>
[hackage-badge]:      <https://img.shields.io/hackage/v/icloud-auth.svg>
[hackage]:            <https://hackage.haskell.org/package/icloud-auth>
[iCloud]:             <https://www.icloud.com/>
