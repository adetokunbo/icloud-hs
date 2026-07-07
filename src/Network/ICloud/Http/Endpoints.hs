{- |
Module      : Network.ICloud.Http.Endpoints
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Base URLs for the iCloud HTTP API, selected by region.

iCloud operates two endpoint sets depending on the user's region:

* 'Usual' — targets @icloud.com@; for users outside mainland China.
* 'China' — targets @icloud.com.cn@; required for mainland China accounts.

Use 'realmEndpoints' to obtain the 'Endpoints' for the appropriate 'Realm',
then pass the result to 'Network.ICloud.Http.mkApi' or supply it directly to
'Network.ICloud.Http.mkApiWith'.
-}
module Network.ICloud.Http.Endpoints
  ( -- * Region selection
    Realm (..)
  , realmEndpoints

    -- * Endpoint bundle
  , Endpoints (..)
  )
where

import Network.ICloud.Internal.Endpoints (Endpoints (..), Realm (..), realmEndpoints)

