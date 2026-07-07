{- |
Module      : Network.ICloud.Http.Endpoints
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Defines 'Endpoints', a datatype representing the HTTP base URLs used by the
ICloud API, along with 'Realm' for selecting a domain.
-}
module Network.ICloud.Http.Endpoints
  ( Realm
  , realmEndpoints
  , Endpoints (..)
  )
where

import Network.ICloud.Http.Endpoints.Internal (Endpoints (..), Realm, realmEndpoints)

