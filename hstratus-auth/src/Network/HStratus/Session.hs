{-# OPTIONS_GHC -Wno-missing-home-modules #-}

{- |
Module      : Network.HStratus.Session
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides the core types for an iCloud authentication session.

'Credentials' holds the account ID and password used to sign in. A 'Session'
combines those credentials with a filesystem working directory (where cookies,
session tokens, and account state are persisted between runs) and a per-client
identifier. 'AccountData' carries the account information returned by the
account-login endpoint, including the HSA version that determines which
two-factor challenge flow applies.

Use 'loadSession' to initialise a session from the local filesystem. The
credentials file is read from @$XDG_CONFIG_HOME\/hstratus\/credentials.json@;
other session state is created in the same directory on first use.

The session value is then passed to 'Network.HStratus.Http.mkApiWith' (or
'Network.HStratus.Http.mkApi' for the default configuration) to construct an
'Network.HStratus.Http.Api' handle for making authenticated requests.
-}
module Network.HStratus.Session
  ( -- * Credentials

    {- | The account ID and password used to sign in to iCloud.

    Expected to be read from
    @$XDG_CONFIG_HOME\/hstratus\/credentials.json@ with the fields
    @accountName@ and @password@.
    -}
    Credentials (..)

    -- * Session

    {- | Persistent data identifying a user and their local authentication state.

    Holds the credentials used to authenticate, the directory where session files
    are stored (cookies, saved headers, account data), and the per-client OAuth
    state identifier.
    -}
  , Session (..)

    -- * AccountData

    {- | Structured account information returned by the account-login endpoint.

    The 'adHsaVersion' field determines which two-factor flow applies:

    * @0@ — unknown (used as a sentinel when no account data is available)
    * @1@ — legacy two-step authentication (2SA); handled via the setup endpoint
    * @2@ — modern two-factor authentication (2FA); handled via the auth endpoint
    -}
  , Webservice (..)
  , AccountData (..)

    -- * Loading a session

    {- | Load a 'Session' from the local filesystem.

    Reads 'Credentials' from
    @$XDG_CONFIG_HOME\/hstratus\/credentials.json@ and initialises the
    session working directory (creating it if absent). A per-client ID is read
    from disk if one exists, or generated and saved for future runs.

    Throws an 'IOError' if the credentials file is absent or cannot be parsed.
    -}
  , loadSession

    -- * Saving credentials

    {- | Write 'Credentials' to @$XDG_CONFIG_HOME\/hstratus\/credentials.json@,
    creating the directory if it does not exist.
    -}
  , saveCredentials
  )
where

import Network.HStratus.Internal.Session
  ( AccountData (..)
  , Credentials (..)
  , Session (..)
  , Webservice (..)
  , loadSession
  , saveCredentials
  )

