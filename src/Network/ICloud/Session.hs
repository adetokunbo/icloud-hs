{-# OPTIONS_GHC -Wno-missing-home-modules #-}

{- |
Module      : Network.ICloud.Session
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Contains data types and combinators for persisting authentication data.
-}
module Network.ICloud.Session
  ( -- * Credentials
    Credentials (..)

    -- * Session
  , Session (..)

    -- * AccountData
  , AccountData (..)

    -- * load
  , loadSession
  )
where

import Network.ICloud.Session.Internal
  ( AccountData (..)
  , Credentials (..)
  , Session (..)
  , loadSession
  )

