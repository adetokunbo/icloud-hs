{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.HStratus.Http.Common
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Shared HTTP utilities for iCloud service clients.

Re-exported from "Network.HStratus.Internal.Endpoints" for use by downstream
libraries (@icloud-drive@, @icloud-notes@, etc.) that need to build
authenticated service requests.
-}
module Network.HStratus.Http.Common
  ( -- * Headers
    icloudHome
  , icloudBrowserHeaders
  , withHeaders

    -- * Request helpers
  , stripTrailingSlash
  , lookupWebservice
  )
where

import Network.HStratus.Internal.Endpoints
  ( icloudBrowserHeaders
  , icloudHome
  , lookupWebservice
  , stripTrailingSlash
  , withHeaders
  )

