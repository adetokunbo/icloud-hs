{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.ICloud.Internal.Endpoints
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Network.ICloud.Internal.Endpoints
  ( -- * Types
    Endpoints (..)
  , Realm (..)

    -- * Region selection
  , realmEndpoints

    -- * Request builders
  , signinInitBase
  , signinCompleteBase
  , validateBase
  , accountLoginBase
  , twoSvTrust
  , verifySecurityCodeReq
  , validateVerification
  , sendVerification
  , listDevices

    -- * Request modifiers
  , extendPath
  , toPut
  , withHeaders
  , withBody
  , withAcceptJson
  , withICloudWidgetKey
  , withAppleOauthHeaders

    -- * Header helpers
  , homeHeaders
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.String.Conv (toS)
import Data.Text (Text)
import Network.HTTP.Client
  ( Request (..)
  , RequestBody (..)
  , defaultRequest
  )
import Network.HTTP.Types
  ( Header
  , HeaderName
  , RequestHeaders
  , hAccept
  , hContentType
  , hReferer
  , hUserAgent
  , methodGet
  , methodPost
  , methodPut
  )


-- | @RequestHeaders@ that include the @Endpoint@ @home@
homeHeaders :: Endpoints -> RequestHeaders
homeHeaders ep = [(hOrigin, epHome ep), (hReferer, epHome ep <> "/")]


commonHeaders :: Endpoints -> RequestHeaders
commonHeaders ep = userAgent : homeHeaders ep


-- | Construct a new @Request@ with that path changed by adding a suffix
extendPath :: Request -> ByteString -> Request
extendPath req suffix = req{path = path req <> suffix}


-- | Construct a new @Request@ with the method changed to @GET@
toGet :: Request -> Request
toGet req = req{method = methodGet}


-- | Construct a new @Request@ with the method changed to @PUT@
toPut :: Request -> Request
toPut req = req{method = methodPut}


{- | Base URLs and default request templates for the iCloud HTTP API.

Passed to the request-building functions inside 'Network.ICloud.Http' to
construct properly-targeted API calls.
-}
data Endpoints = Endpoints
  { epHome :: !ByteString
  -- ^ home origin, e.g. @https://www.icloud.com@; used in @Origin@ and @Referer@ headers
  , epAuth :: !Request
  -- ^ base request for the authentication endpoint (@idmsa.apple.com@)
  , epSetup :: !Request
  -- ^ base request for the setup\/account endpoint (@setup.icloud.com@)
  }


{- | The two regional iCloud endpoint sets.

* 'Usual' — @icloud.com@ family; for users outside mainland China.
* 'China' — @icloud.com.cn@ family; required for mainland China accounts.
-}
data Realm = China | Usual
  deriving (Eq, Show)


-- | Return the 'Endpoints' for the given 'Realm'.
realmEndpoints :: Realm -> Endpoints
realmEndpoints China = chinaEndpoints
realmEndpoints Usual = usualEndpoints


usualEndpoints :: Endpoints
usualEndpoints =
  Endpoints
    { epHome = "https://www.icloud.com"
    , epAuth = authReq
    , epSetup = setupReq
    }


chinaEndpoints :: Endpoints
chinaEndpoints =
  Endpoints
    { epHome = "https://www.icloud.com.cn"
    , epAuth = authReq
    , epSetup = setupReq{host = "setup.icloud.com.cn"}
    }


apiRequest :: Request
apiRequest =
  defaultRequest
    { secure = True
    , port = 443
    , method = methodPost
    , requestHeaders = [(hAccept, "application/json"), (hContentType, "application/json")]
    }


authReq :: Request
authReq = apiRequest{host = "idmsa.apple.com", path = "/appleauth/auth"}


setupReq :: Request
setupReq = apiRequest{host = "setup.icloud.com", path = "/setup/ws/1"}


appleOauthHeaders :: [Header]
appleOauthHeaders =
  [ ("X-Apple-OAuth-Client-Id", iCloudKey)
  , ("X-Apple-OAuth-Client-Type", "firstPartyAuth")
  , ("X-Apple-OAuth-Redirect-URI", "https://www.icloud.com")
  , ("X-Apple-OAuth-Require-Grant-Code", "true")
  , ("X-Apple-OAuth-Response-Mode", "web_message")
  , ("X-Apple-OAuth-Response-Type", "code")
  , ("X-Apple-Widget-Key", iCloudKey)
  ]


iCloudKey :: ByteString
iCloudKey = "d39ba9916b7251055b22c7f910e2ea796ee65e98b2ddecea8f5dde8d9d1a815d"


browserAgent :: ByteString
browserAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36"


userAgent :: Header
userAgent = (hUserAgent, browserAgent)


acceptJson :: Header
acceptJson = (hAccept, "application/json")


-- | build the basic @Request@ to make the initiate signin
signinInitBase :: Endpoints -> Request
signinInitBase =
  let
    withQuery x = x{queryString = "?isRememberMeEnabled=true"}
   in
    withQuery . (`extendPath` "/signin/init") . epAuth


-- | build the basic @Request@ to complete signin
signinCompleteBase :: Endpoints -> Request
signinCompleteBase = (`extendPath` "/signin/complete") . epAuth


-- | build the basic @Request@ to that validates user credentials
validateBase :: Endpoints -> Request
validateBase ep = withHeaders (commonHeaders ep) $ (`extendPath` "/validate") $ epSetup ep


-- | build the basic @Request@ to that performs login
accountLoginBase :: Endpoints -> Request
accountLoginBase = (`extendPath` "/accountLogin") . epSetup


-- | build the basic @Request@ to that makes the saved credentials trusted
twoSvTrust :: Endpoints -> Request
twoSvTrust = (`extendPath` "/2sv/trust") . toGet . epAuth


-- | build the @Request@ to that verifies makes a security code
verifySecurityCodeReq :: Text -> Endpoints -> Request
verifySecurityCodeReq codeType =
  (`extendPath` ("/verify/" <> toS codeType <> "/securitycode"))
    . withHeaders [(hAccept, "application/json"), (hContentType, "application/json")]
    . epAuth


validateVerification :: Endpoints -> Request
validateVerification = (`extendPath` "/validateVerificationCode") . epSetup


sendVerification :: Endpoints -> Request
sendVerification = (`extendPath` "/sendVerificationCode") . epSetup


listDevices :: Endpoints -> Request
listDevices = (`extendPath` "/listDevices") . toGet . epSetup


withHeaders :: RequestHeaders -> Request -> Request
withHeaders newHeaders req = req{requestHeaders = newHeaders <> requestHeaders req}


withBody :: LBS.LazyByteString -> Request -> Request
withBody b req = req{requestBody = RequestBodyLBS b}


hOrigin :: HeaderName
hOrigin = mk "Origin"


withAcceptJson :: RequestHeaders -> RequestHeaders
withAcceptJson = (acceptJson :)


withICloudWidgetKey :: RequestHeaders -> RequestHeaders
withICloudWidgetKey = (("X-Apple-Widget-Key", iCloudKey) :)


withAppleOauthHeaders :: RequestHeaders -> RequestHeaders
withAppleOauthHeaders = (appleOauthHeaders <>)
