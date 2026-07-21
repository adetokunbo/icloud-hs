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
  , twoFaOptionsBase
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
  , withWidgetKey
  , withAppleOauthHeaders

    -- * Header helpers
  , homeHeaders
  , icloudBrowserHeaders

    -- * Shared service utilities
  , icloudHome
  , stripTrailingSlash
  , lookupWebservice
  )
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String.Conv (toS)
import Data.Text (Text)
import Network.HTTP.Client
  ( Request (..)
  , RequestBody (..)
  , defaultRequest
  , parseRequest
  )
import Network.HTTP.Types
  ( Header
  , HeaderName
  , RequestHeaders
  , hAccept
  , hReferer
  , hUserAgent
  , methodGet
  , methodPost
  , methodPut
  )
import Network.ICloud.Internal.HttpErrors (AuthError (..))
import Network.ICloud.Internal.Session (Webservice (..))


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


data Endpoints = Endpoints
  { epHome :: !ByteString
  -- ^ home origin, e.g. @https://www.icloud.com@; used in @Origin@ and @Referer@ headers
  , epAuth :: !Request
  -- ^ base request for the authentication endpoint (@idmsa.apple.com@)
  , epSetup :: !Request
  -- ^ base request for the setup\/account endpoint (@setup.icloud.com@)
  , epWidgetKey :: !ByteString
  -- ^ value sent as @X-Apple-Widget-Key@ and @X-Apple-OAuth-Client-Id@; override if Apple rotates it
  }


data Realm = China | Usual
  deriving (Eq, Show)


realmEndpoints :: Realm -> Endpoints
realmEndpoints China = chinaEndpoints
realmEndpoints Usual = usualEndpoints


-- | The iCloud home origin used in @Origin@ and @Referer@ headers.
icloudHome :: ByteString
icloudHome = "https://www.icloud.com"


usualEndpoints :: Endpoints
usualEndpoints =
  Endpoints
    { epHome = icloudHome
    , epAuth = authReq
    , epSetup = setupReq
    , epWidgetKey = iCloudKey
    }


chinaEndpoints :: Endpoints
chinaEndpoints =
  Endpoints
    { epHome = "https://www.icloud.com.cn"
    , epAuth = authReq
    , epSetup = setupReq{host = "setup.icloud.com.cn"}
    , epWidgetKey = iCloudKey
    }


apiRequest :: Request
apiRequest =
  defaultRequest
    { secure = True
    , port = 443
    , method = methodPost
    }


authReq :: Request
authReq = apiRequest{host = "idmsa.apple.com", path = "/appleauth/auth"}


setupReq :: Request
setupReq = apiRequest{host = "setup.icloud.com", path = "/setup/ws/1"}


appleOauthHeaders :: ByteString -> [Header]
appleOauthHeaders key =
  [ ("X-Apple-OAuth-Client-Id", key)
  , ("X-Apple-OAuth-Client-Type", "firstPartyAuth")
  , ("X-Apple-OAuth-Redirect-URI", "https://www.icloud.com")
  , ("X-Apple-OAuth-Require-Grant-Code", "true")
  , ("X-Apple-OAuth-Response-Mode", "web_message")
  , ("X-Apple-OAuth-Response-Type", "code")
  , ("X-Apple-Widget-Key", key)
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
signinInitBase = (`extendPath` "/signin/init") . epAuth


-- | build the basic @Request@ to complete signin
signinCompleteBase :: Endpoints -> Request
signinCompleteBase =
  let
    withQuery x = x{queryString = "isRememberMeEnabled=true"}
   in
    withQuery . (`extendPath` "/signin/complete") . epAuth


-- | build the basic @Request@ to that validates user credentials
validateBase :: Endpoints -> Request
validateBase ep = withHeaders (commonHeaders ep) $ (`extendPath` "/validate") $ epSetup ep


-- | build the basic @Request@ to that performs login
accountLoginBase :: Endpoints -> Request
accountLoginBase = (`extendPath` "/accountLogin") . epSetup


-- | build the basic @Request@ to that makes the saved credentials trusted
twoSvTrust :: Endpoints -> Request
twoSvTrust = (`extendPath` "/2sv/trust") . toGet . epAuth


-- | build the @Request@ to fetch the 2FA options after the 409 from signin/complete
twoFaOptionsBase :: Endpoints -> Request
twoFaOptionsBase = toGet . epAuth


-- | build the @Request@ to that verifies makes a security code
verifySecurityCodeReq :: Text -> Endpoints -> Request
verifySecurityCodeReq codeType =
  (`extendPath` ("/verify/" <> toS codeType <> "/securitycode")) . epAuth


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


withWidgetKey :: ByteString -> RequestHeaders -> RequestHeaders
withWidgetKey key = (("X-Apple-Widget-Key", key) :)


withAppleOauthHeaders :: ByteString -> RequestHeaders -> RequestHeaders
withAppleOauthHeaders key = (appleOauthHeaders key <>)


-- | Standard browser-style headers sent with every iCloud service request.
icloudBrowserHeaders :: RequestHeaders
icloudBrowserHeaders =
  [ acceptJson
  , userAgent
  , (hOrigin, icloudHome)
  , (hReferer, icloudHome <> "/")
  ]


-- | Strip a trailing @/@ from a strict 'ByteString' path.
stripTrailingSlash :: ByteString -> ByteString
stripTrailingSlash bs
  | not (BS8.null bs) && BS8.last bs == '/' = BS8.init bs
  | otherwise = bs


{- | Look up a service URL by key in the webservices map and parse it into a
'Request'.  Fails with an informative message if the key is absent.
-}
lookupWebservice :: Text -> Map Text Webservice -> IO Request
lookupWebservice key ws =
  case Map.lookup key ws of
    Nothing -> throwIO $ WebserviceNotFound key
    Just (Webservice _ (Just "inactive")) -> throwIO $ WebserviceNotFound key
    Just (Webservice url _) -> parseRequest (toS url)
