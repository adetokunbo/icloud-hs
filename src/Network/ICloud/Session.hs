{-# OPTIONS_GHC -Wno-missing-home-modules #-}

{- |
Module      : Network.ICloud.Session
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides the core types for an iCloud authentication session.

'Credentials' holds the Apple ID and password used to sign in. A 'Session'
combines those credentials with a filesystem working directory (where cookies,
session tokens, and account state are persisted between runs) and a per-client
identifier. 'AccountData' carries the account information returned by the
account-login endpoint, including the HSA version that determines which
two-factor challenge flow applies.

Use 'loadSession' to initialise a session from the local filesystem. The
credentials file is read from @$XDG_CONFIG_HOME\/hs-icloud-auth\/credentials.json@;
other session state is created in the same directory on first use.

The session value is then passed to 'Network.ICloud.Http.mkApiWith' (or
'Network.ICloud.Http.mkApi' for the default configuration) to construct an
'Network.ICloud.Http.Api' handle for making authenticated requests.
-}
module Network.ICloud.Session
  ( -- * Credentials
    Credentials (..)

    -- * Session
  , Session (..)

    -- * AccountData
  , AccountData (..)

    -- * Loading a session
  , loadSession
  )
where

import Network.ICloud.Internal.Session
  ( AccountData (..)
  , Credentials (..)
  , Session (..)
  , loadSession
  )

