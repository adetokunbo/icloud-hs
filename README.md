# icloud-auth - unofficial auth for iCloud services

[![GitHub CI](https://github.com/adetokunbo/icloud-auth/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/icloud-auth/actions)
[![Stackage Nightly](http://stackage.org/package/icloud-auth/badge/nightly)](http://stackage.org/nightly/package/icloud-auth)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/icloud-auth/blob/master/LICENSE)

`icloud-auth` allows logon to [iCloud] servers; upon provision of the username and
password, it retrieves and stores an authorization token, for use in programmatic
sessions with other iCloud services


## Warning - use at your own risk

- It is unofficial, *not* supported by Apple, and not guaranteed to work.

- The iCloud service APIs it uses are not officially documented and can be
changed at any time. Use at your own risk; in the worst case, Apple might ban
your account!


### Regular reauthentication 

Note: The retrieved authorization token expires, after which re-authentication
is required. There is no way to control to the duratiion, it is set by iCloud
itself. At the time of writing (2024/10). the duration is two months.


### Usage

#### Securely store your access credentials

Store your Apple ID and password in XDG-CONFIG-HOME/hs-icloud-auth/credential.json
E.g, use the following bash snippet, updating it with your username and password
accordingly

```
$ ICLOUD_AUTH_CONF="${XDG_CONFIG_HOME:=${HOME}/.config}/hs-icloud-auth"
$ mkdir -p $ICLOUD_AUTH_CONF
$ cat << EOF > $ICLOUD_AUTH_CONF/credential.json
{ 
  "accountName":  "your-username",
  "password": "your-password"
}
EOF
```

Ensure the 'credential.json' is only readable by you:

```
$ chmod 600 $XDG_CONFIG_HOME/hs-icloud-auth/credential.json
```

### Design Details

#### From [icloudpy][]

- obtain the cookie and session file basenames using spruced accountName
  - the spruced accountName the accountName filtered using Char#isAlphaNum

#### data types

```haskell

-- | don't derive Show to avoid the risk of logging a password
data Credentials = Credentials
  { credAccountName :: !Text
  , credPassword    :: !Text
  } deriving (Eq)


-- | don't derive Show to avoid the risk of logging a password
data Session = Session
  { sessionCreds :: !Credentials
  , sessionTopDir :: !FilePath!
  } deriving (Eq)

data SessionData = SessionData
  { sdAccountCountry :: !(Maybe Text)
  , sdSessionId      :: !(Maybe Text)
  , sdSessionToken   :: !(Maybe Text)
  , sdCounter        :: !(Maybe Text)
  } deriving (Eq, Show)
```

#### states

```haskell

```

[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/icloud-auth.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=icloud-auth>
[hackage-badge]:      <https://img.shields.io/hackage/v/icloud-auth.svg>
[hackage]:            <https://hackage.haskell.org/package/icloud-auth>
[iCloud]:             <https://www.icloud.com/>
[icloudpy]:           <https://github.com/mandarons/icloudpy">
