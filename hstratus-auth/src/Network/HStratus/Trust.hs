{- |
Module      : Network.HStratus.Trust
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
defaults in 'Network.HStratus.Http.login' and 'Network.HStratus.Http.complete2SA'.
Supply your own alternatives via 'Network.HStratus.Http.loginWith' and
'Network.HStratus.Http.complete2SAWith' for testing or automation.
-}
module Network.HStratus.Trust
  ( -- * Two-factor challenge data

    {- | The two-factor challenge data returned by the auth endpoint after SRP sign-in.

    Describes which trusted contacts are available to receive a verification code
    ('tdList'), the current state of the security-code gate ('tdSecurityCode'),
    and whether any trusted devices are registered ('tdNoTrustedDevices').
    -}
    TrustData (..)
    -- | Information about a trusted phone number.
  , TrustedPhone (..)

    -- * Legacy two-step device

    {- | A 2SA device from the setup endpoint.

    Stored as the raw JSON object so the entire dict can be echoed back to
    @sendVerificationCode@ and augmented for @validateVerificationCode@.
    -}
  , Setup2SADevice (..)
    {- | Extract a human-readable label from a 'Setup2SADevice', preferring
    @phoneNumber@ then @name@.
    -}
  , setup2SADeviceLabel

    -- * Interactive prompts

    {- | Interactively prompt the user to enter the verification code sent to
    their trusted phone or device. The first argument is the expected code
    length, used to make the prompt more specific (e.g. @"6-digit"@).

    Used as the default code-reading action in 'Network.HStratus.Http.login'.
    Supply an alternative via 'Network.HStratus.Http.loginWith' for testing or
    automation.
    -}
  , pleaseReadCode
    {- | Interactively prompt the user to choose between device push and SMS
    for HSA2 2FA.

    If no trusted devices are registered ('tdNoTrustedDevices' is @True@), the
    first trusted phone is selected automatically. Otherwise, the user is
    prompted to press Enter for device push or enter a number to receive an SMS
    code.
    -}
  , selectTwoFaPhone
    {- | Interactively prompt the user to select a device from a list of 2SA
    setup devices.

    Used as the default device-selection action in
    'Network.HStratus.Http.complete2SA'. Supply an alternative via
    'Network.HStratus.Http.complete2SAWith' for testing or automation.
    -}
  , selectSetupDevice
  )
where

import Network.HStratus.Internal.Trust
  ( Setup2SADevice (..)
  , TrustData (..)
  , TrustedPhone (..)
  , pleaseReadCode
  , selectSetupDevice
  , selectTwoFaPhone
  , setup2SADeviceLabel
  )

