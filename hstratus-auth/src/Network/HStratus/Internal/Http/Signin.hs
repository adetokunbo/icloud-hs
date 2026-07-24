{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.HStratus.Internal.Http.Signin
  ( -- * Trust data
    fetchTrustData

    -- * SMS code
  , requestSmsCode
  , verifySmsCode

    -- * Session validation
  , validate

    -- * SRP sign-in
  , runSigninInit
  , runSigninComplete

    -- * Account login
  , accountLogin

    -- * 2FA helpers
  , triggerTwoFaPush
  , doTrustStep
  , verifyTwoFaCode

    -- * 2SA helpers
  , listSetupDevices
  , sendSetupVerification
  , validateSetupVerification
  )
where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (unless, void)
import Crypto.SRP
  ( FromClient (..)
  , FromServer (..)
  , Results (..)
  )
import Data.Aeson
  ( FromJSON (..)
  , Object
  , encode
  , withObject
  , (.:)
  )
import Data.Aeson.Types (Parser, Value (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Maybe (fromMaybe)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word64)
import Network.HStratus.Http.Endpoints (Endpoints (..))
import Network.HStratus.Internal.Endpoints
  ( accountLoginBase
  , extendPath
  , homeHeaders
  , listDevices
  , sendVerification
  , signinCompleteBase
  , signinInitBase
  , toPut
  , twoFaOptionsBase
  , twoSvTrust
  , validateBase
  , validateVerification
  , verifySecurityCodeReq
  , withAcceptJson
  , withBody
  , withHeaders
  )
import Network.HStratus.Internal.Http
  ( KeyDeriver (..)
  , PasswordProtocol (..)
  , phoneCodeBody
  , phoneTriggerBody
  , validateSetupBody
  )
import Network.HStratus.Internal.Http.Api
  ( Api (..)
  , AuthCode
  , asObject
  , authHeaders
  , callApi
  , callHandlingResponse
  , callRequiredHeaders
  , extractOr'
  , maybeValue
  , mkJsonRequest
  , rawRequest
  , requiredHeaders
  , showStatusOf
  , withJsonRequestHeaders
  )
import Network.HStratus.Internal.HttpErrors
  ( ApiResponse
  , AuthError (..)
  )
import Network.HStratus.Internal.Session
  ( SavedHeaders (..)
  , loadSavedHeaders
  )
import Network.HStratus.Internal.Trust (TrustData (..), TrustedPhone)
import Network.HStratus.Session (Credentials (..), Session (..))
import Network.HStratus.Trust (Setup2SADevice (..))
import Network.HTTP.Client (Request (..), Response (..))
import Network.HTTP.Types (Status (..))


-- | Fetch the 2FA options immediately after the 409 from signin/complete
fetchTrustData :: Api -> IO TrustData
fetchTrustData api = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req = withHeaders (withAcceptJson $ authHeaders api savedHdrs) (twoFaOptionsBase (apiEndpoints api))
  callApi api req >>= extractOr'


-- | POST to phone/securitycode to request an SMS code to the given phone
requestSmsCode :: Api -> TrustedPhone -> IO ()
requestSmsCode api@Api{apiEndpoints = ep} tp = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req =
        withHeaders (authHeaders api savedHdrs) $
          withJsonRequestHeaders $
            withBody (encode $ phoneTriggerBody tp) $
              toPut (extendPath (epAuth ep) "/verify/phone")
  resp <- rawRequest api req
  unless (statusCode (responseStatus resp) < 400) $
    throwIO $
      UnexpectedResponse $
        showStatusOf resp


-- | POST to phone/securitycode to verify an SMS code; returns True when accepted
verifySmsCode :: Api -> TrustedPhone -> AuthCode -> IO Bool
verifySmsCode api@Api{apiEndpoints = ep} tp code = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req =
        withHeaders (authHeaders api savedHdrs) $
          withJsonRequestHeaders $
            withBody (encode $ phoneCodeBody tp code) $
              verifySecurityCodeReq "phone" ep
  resp <- rawRequest api req
  let c = statusCode (responseStatus resp)
  if
    | c < 400 -> pure True
    | c == 400 -> pure False
    | otherwise -> throwIO $ UnexpectedResponse $ showStatusOf resp


data SigninInitReply = SigninInitReply
  { sirTag :: !Text
  , sirProtocol :: !PasswordProtocol
  , sirPublicBytes :: !ByteString
  , sirIterations :: !Word64
  , sirSalt :: !ByteString
  }
  deriving (Eq, Show)


instance FromJSON SigninInitReply where
  parseJSON = withObject "SigninInitReply" parseSigninInitReply


parseSigninInitReply :: Object -> Parser SigninInitReply
parseSigninInitReply o =
  let tag = o .: "c"
      iterations = o .: "iteration"
      protocol = o .: "protocol"
      publicBytes = o .: "b" >>= parseBase64Bytes
      salt = o .: "salt" >>= parseBase64Bytes
      parseBase64Bytes s = case B64.decode (encodeUtf8 s) of
        Left err -> fail err
        Right b -> pure b
   in SigninInitReply
        <$> tag
        <*> protocol
        <*> publicBytes
        <*> iterations
        <*> salt


signinInit :: Api -> FromClient -> IO SigninInitReply
signinInit api other = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  callHandlingResponse signinInitReq (withHeaders (authHeaders api savedHdrs)) api other


runSigninInit :: Api -> FromClient -> IO (FromServer, KeyDeriver)
runSigninInit api fc = do
  r <- signinInit api fc
  let fromServer =
        FromServer
          { fsPublicBytes = sirPublicBytes r
          , fsSalt = sirSalt r
          , fsPrimeGroup = apiGroup api
          , fsKnownAlgorithm = apiHashAlgorithm api
          }
      keyDeriver =
        KeyDeriver
          { kdTag = sirTag r
          , kdIterations = sirIterations r
          , kdWrappedF = apiWrappedPseudoRF api
          , kdProtocol = sirProtocol r
          }
  pure (fromServer, keyDeriver)


signinInitReq :: Endpoints -> FromClient -> Request
signinInitReq = mkJsonRequest signinInitBase signinInitValue


signinInitValue :: FromClient -> Value
signinInitValue fc =
  let a = decodeUtf8 $ B64.encode $ fcPublicBytes fc
   in asObject
        [ ("a", String a)
        , ("accountName", String (fcUser fc))
        , ("protocols", Array ["s2k", "s2k_fo"])
        ]


data SigninCompletion = SigninCompletion
  { siTag :: !Text
  , siAccountName :: !Text
  , siSavedHeaders :: !SavedHeaders
  , siResults :: !Results
  }


runSigninComplete :: Api -> KeyDeriver -> Results -> IO ()
runSigninComplete api@Api{apiSession = session} kd results = do
  siSavedHeaders <- loadSavedHeaders session
  let siAccountName = credAccountName $ sessionCreds session
      completion =
        SigninCompletion
          { siTag = kdTag kd
          , siAccountName
          , siResults = results
          , siSavedHeaders
          }
  signinComplete api completion


signinComplete :: Api -> SigninCompletion -> IO ()
signinComplete api sc = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req = withHeaders (authHeaders api savedHdrs) $ signinCompleteReq (apiEndpoints api) sc
  resp <- callApi api req :: IO (Response (ApiResponse ()))
  handleSigninComplete resp


signinCompleteReq :: Endpoints -> SigninCompletion -> Request
signinCompleteReq = mkJsonRequest signinCompleteBase signinCompleteValue


signinCompleteValue :: SigninCompletion -> Value
signinCompleteValue sc =
  let Results{rClientProof, rServerProof} = siResults sc
      toBase64 = decodeUtf8 . B64.encode
      singleElem x = Array [String x]
      maybeArray = maybe (Array []) singleElem
   in asObject
        [ ("m1", String (toBase64 rClientProof))
        , ("m2", String (toBase64 rServerProof))
        , ("trustTokens", maybeArray (shTrustToken (siSavedHeaders sc)))
        , ("rememberMe", Bool True)
        , ("accountName", String (siAccountName sc))
        , ("c", String (siTag sc))
        ]


handleSigninComplete :: Response (ApiResponse ()) -> IO ()
handleSigninComplete resp = do
  let code = statusCode $ responseStatus resp
  if
    | code == 401 -> throwIO InvalidCredentials
    | code == 403 -> throwIO AccountLocked
    | code == 412 -> throwIO PrivacyAgreementRequired
    | code == 409 -> pure () -- 2FA required; accountLogin will detect it
    | code >= 400 -> throwIO $ UnexpectedResponse $ showStatusOf resp
    | otherwise -> pure ()


validate :: Api -> IO Bool
validate api@Api{apiEndpoints} = do
  resp <- rawRequest api (validateReq apiEndpoints)
  let code = statusCode (responseStatus resp)
  if
    | code == 401 -> pure False
    | code >= 400 -> throwIO $ UnexpectedResponse $ showStatusOf resp
    | otherwise -> pure True


validateReq :: Endpoints -> Request
validateReq = withJsonRequestHeaders . withBody (encode Null) . validateBase


accountLogin :: Api -> IO Value
accountLogin api@Api{apiEndpoints = ep} = do
  savedHdrs <- loadSavedHeaders $ apiSession api
  let hdrs = homeHeaders ep
  callHandlingResponse accountLoginReq (withHeaders hdrs) api savedHdrs


accountLoginReq :: Endpoints -> SavedHeaders -> Request
accountLoginReq = mkJsonRequest accountLoginBase accountLoginValue


triggerTwoFaPush :: Api -> IO ()
triggerTwoFaPush api@Api{apiEndpoints = ep} = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req =
        withHeaders
          (withAcceptJson $ authHeaders api savedHdrs)
          (toPut (extendPath (epAuth ep) "/verify/trusteddevice/securitycode"))
  void (rawRequest api req) `catch` \(e :: IOException) ->
    throwIO (UnexpectedResponse ("2FA push failed: " <> toS (show e)))


doTrustStep :: Api -> IO ()
doTrustStep api@Api{apiEndpoints = ep} = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req = withHeaders (withAcceptJson $ authHeaders api savedHdrs) (twoSvTrust ep)
  resp <- rawRequest api req
  unless (statusCode (responseStatus resp) < 400) $
    throwIO $
      UnexpectedResponse $
        showStatusOf resp


verifyTwoFaCode :: Api -> AuthCode -> IO Bool
verifyTwoFaCode api@Api{apiEndpoints = ep} code = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let body = encode $ Object [("securityCode", Object [("code", String code)])]
      req =
        withHeaders (authHeaders api savedHdrs) $
          withJsonRequestHeaders $
            withBody body $
              verifySecurityCodeReq "trusteddevice" ep
  resp <- rawRequest api req
  let c = statusCode (responseStatus resp)
  if
    | c < 400 -> pure True
    | c == 400 -> pure False
    | otherwise -> throwIO $ UnexpectedResponse $ showStatusOf resp


accountLoginValue :: SavedHeaders -> Value
accountLoginValue hs =
  asObject
    [ ("accountCountryCode", maybeValue String (shCountry hs))
    , ("dsWebAuthToken", maybeValue String (shSessionToken hs))
    , ("trustToken", String $ fromMaybe "" $ shTrustToken hs)
    , ("extended_login", Bool True)
    ]


newtype ListDevicesReply = ListDevicesReply {ldrDevices :: [Setup2SADevice]}


instance FromJSON ListDevicesReply where
  parseJSON = withObject "ListDevicesReply" $ \o -> ListDevicesReply <$> o .: "devices"


listSetupDevices :: Api -> IO [Setup2SADevice]
listSetupDevices api@Api{apiEndpoints = ep} =
  ldrDevices <$> callRequiredHeaders api (listDevices ep)


sendSetupVerification :: Api -> Setup2SADevice -> IO ()
sendSetupVerification api@Api{apiSession = s, apiEndpoints = ep} device = do
  savedHdrs <- loadSavedHeaders s
  let req =
        withHeaders (requiredHeaders (epWidgetKey ep) savedHdrs) $
          withJsonRequestHeaders $
            withBody (encode device) $
              sendVerification ep
  resp <- rawRequest api req
  unless (statusCode (responseStatus resp) < 400) $ throwIO $ UnexpectedResponse $ showStatusOf resp


validateSetupVerification :: Api -> Setup2SADevice -> AuthCode -> IO Bool
validateSetupVerification api@Api{apiSession = s, apiEndpoints = ep} device code = do
  savedHdrs <- loadSavedHeaders s
  let req =
        withHeaders (requiredHeaders (epWidgetKey ep) savedHdrs) $
          withJsonRequestHeaders $
            withBody (encode $ validateSetupBody device code) $
              validateVerification ep
  resp <- rawRequest api req
  pure $ statusCode (responseStatus resp) < 400
