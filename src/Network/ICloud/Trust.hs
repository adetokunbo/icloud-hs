{- |
Module      : Network.ICloud.Trust
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Types and utilities for handling iCloud two-factor trust challenges.

After a successful SRP sign-in, iCloud may require an additional verification
step. 'TrustData' describes the challenge: which trusted phones or devices are
available to receive a code, and the current state of the security-code gate.

Two challenge flows exist:

* __2FA__ (modern, HSA version ≥ 2): the auth endpoint issues a 'TrustData'
  challenge; the user enters a code sent to a trusted phone or device.

* __2SA__ (legacy, HSA version 1): the setup endpoint lists registered
  'Setup2SADevice' values; the user selects one to receive a code.

'pleaseReadCode' and 'selectSetupDevice' are interactive prompts used as
defaults in 'Network.ICloud.Http.login' and 'Network.ICloud.Http.complete2SA'.
Supply your own alternatives via 'Network.ICloud.Http.loginWith' and
'Network.ICloud.Http.complete2SAWith' for testing or automation.
-}
module Network.ICloud.Trust
  ( -- * Two-factor challenge data
    TrustData (..)

    -- * Legacy two-step device
  , Setup2SADevice (..)
  , setup2SADeviceLabel

    -- * Interactive prompts
  , pleaseReadCode
  , selectSetupDevice
  )
where

import Network.ICloud.Internal.Trust
  ( Setup2SADevice (..)
  , TrustData (..)
  , pleaseReadCode
  , selectSetupDevice
  , setup2SADeviceLabel
  )

