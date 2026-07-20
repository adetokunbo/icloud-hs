{- |
Module      : Network.ICloud.Http
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

High-level HTTP client for the iCloud authentication API.

= Typical usage

Create an 'Api' handle with 'mkApi', choosing the 'Realm' that matches the
user's region.  Then call 'login' to run the full sign-in flow: SRP credential
exchange followed by any required two-factor ('completeTwoFactor') or two-step
('complete2SA') challenge, then the account-login request.  On success 'login'
returns 'Authenticated' carrying the refreshed 'Session' and 'AccountData'.

= Injectable alternatives

'login' resolves 2FA and 2SA challenges interactively using the prompts from
"Network.ICloud.Trust".  Pass your own code-reader and device-selector to
'loginWith' to bypass the interactive prompts — useful in tests or automation.

If you already hold a 'Requires2FA' or 'Requires2SA' result from a prior call,
resume the flow with 'completeTwoFactor' \/ 'completeTwoFactorWith' or
'complete2SA' \/ 'complete2SAWith'.
-}
module Network.ICloud.Http
  ( -- * API handle
    mkApi
  , mkApiWith

    -- * Login
  , login
  , loginWith

    -- * Fetching two-factor options
  , fetchTrustData

    -- * SMS phone code
  , requestSmsCode
  , verifySmsCode

    -- * Completing two-factor challenges
  , completeTwoFactor
  , completeTwoFactorWith

    -- * Completing two-step challenges
  , complete2SA
  , complete2SAWith

    -- * Types
  , Api
  , AuthState (..)
  , ApiLogger

    -- * Authenticated HTTP
  , rawRequest

    -- * Logging
  , withLogger
  , fileLogger
  , verboseLogger

    -- * Errors
  , AuthError (..)
  )
where

import Network.ICloud.Internal.Http.Api
  ( Api
  , ApiLogger
  , fileLogger
  , mkApi
  , mkApiWith
  , rawRequest
  , verboseLogger
  , withLogger
  )
import Network.ICloud.Internal.Http.Login
  ( AuthState (..)
  , complete2SA
  , complete2SAWith
  , completeTwoFactor
  , completeTwoFactorWith
  , login
  , loginWith
  )
import Network.ICloud.Internal.Http.Signin
  ( fetchTrustData
  , requestSmsCode
  , verifySmsCode
  )
import Network.ICloud.Internal.HttpErrors (AuthError (..))

