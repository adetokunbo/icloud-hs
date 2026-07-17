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

    {- | The two regional iCloud endpoint sets.

    * 'Usual' — @icloud.com@ family; for users outside mainland China.
    * 'China' — @icloud.com.cn@ family; required for mainland China accounts.
    -}
    Realm (..)
    -- | Return the 'Endpoints' for the given 'Realm'.
  , realmEndpoints

    -- * Endpoint bundle

    {- | Base URLs and default request templates for the iCloud HTTP API.

    Passed to 'Network.ICloud.Http.mkApi' or 'Network.ICloud.Http.mkApiWith'
    to construct properly-targeted API calls.
    -}
  , Endpoints (..)
  )
where

import Network.ICloud.Internal.Endpoints (Endpoints (..), Realm (..), realmEndpoints)

