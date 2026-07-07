{- |
Module      : Network.ICloud.Trust
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Contains data types and combinators for handling ICloud trust challenges.
-}
module Network.ICloud.Trust
  ( -- * data types
    TrustData (..)
  , Setup2SADevice (..)

    -- * functions
  , pleaseReadCode
  , setup2SADeviceLabel
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

