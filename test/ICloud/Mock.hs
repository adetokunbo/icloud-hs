{-# LANGUAGE OverloadedStrings #-}

module ICloud.Mock
  ( Scenario (..)
  , SrpOutcome (..)
  , defaultScenario
  , withMockApp
  , withMockAppCapturing
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Network.HTTP.Types (HeaderName, RequestHeaders, hContentType, status200, status204, status400, status401, status404, status409)
import Network.Wai (Application, pathInfo, rawPathInfo, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Paths_icloud_auth (getDataFileName)


data SrpOutcome = SrpOk | SrpNeeds2FA
  deriving (Eq, Show)


data Scenario = Scenario
  { snValidate :: Bool
  , snSrpOutcome :: SrpOutcome
  , snValidateCodeFails :: Bool
  -- ^ when True, the first validateVerificationCode call returns 400
  , snAccountLoginNeeds2SA :: Bool
  -- ^ when True, the first accountLogin call returns a 2SA-required response
  , snAccountLoginNeeds2FA :: Int
  -- ^ countdown: serve login_2fa_test.json while > 0, then loginWorking
  , snSrpCompleteEmptyError :: Bool
  -- ^ when True, signin/complete returns 401 with no body or Content-Type
  }


defaultScenario :: Scenario
defaultScenario =
  Scenario
    { snValidate = True
    , snSrpOutcome = SrpOk
    , snValidateCodeFails = False
    , snAccountLoginNeeds2SA = False
    , snAccountLoginNeeds2FA = 0
    , snSrpCompleteEmptyError = False
    }


jsonHeaders :: [(HeaderName, ByteString)]
jsonHeaders = [(hContentType, "application/json")]


withMockApp :: Scenario -> (Int -> IO a) -> IO a
withMockApp scenario action =
  withMockAppCapturing scenario $ \port _ -> action port


withMockAppCapturing :: Scenario -> (Int -> IORef [(ByteString, RequestHeaders)] -> IO a) -> IO a
withMockAppCapturing scenario action = do
  srpInit <- LBS.readFile =<< getDataFileName "testdata/srp_init_ok_test.json"
  login2fa <- LBS.readFile =<< getDataFileName "testdata/login_2fa_test.json"
  login2sa <- LBS.readFile =<< getDataFileName "testdata/login_2sa_test.json"
  loginWorking <- LBS.readFile =<< getDataFileName "testdata/login_working_test.json"
  listDevices <- LBS.readFile =<< getDataFileName "testdata/trusted_devices_test.json"
  codeAttemptsRef <- newIORef (0 :: Int)
  accountLoginRef <- newIORef (0 :: Int)
  capturedRef <- newIORef []
  testWithApplication
    ( pure $
        mockApp
          scenario
          capturedRef
          srpInit
          login2fa
          login2sa
          loginWorking
          listDevices
          codeAttemptsRef
          accountLoginRef
    )
    (\port -> action port capturedRef)


mockApp
  :: Scenario
  -> IORef [(ByteString, RequestHeaders)]
  -> LBS.ByteString
  -> LBS.ByteString
  -> LBS.ByteString
  -> LBS.ByteString
  -> LBS.ByteString
  -> IORef Int
  -> IORef Int
  -> Application
mockApp
  scenario
  capturedRef
  srpInit
  login2fa
  login2sa
  loginWorking
  listDevices
  codeAttemptsRef
  accountLoginRef
  req
  respond = do
    modifyIORef' capturedRef ((rawPathInfo req, requestHeaders req) :)
    let method = requestMethod req
        segs = pathInfo req
        json st body = responseLBS st jsonHeaders body
    resp <- case (method, segs) of
      ("POST", ["appleauth", "auth", "signin", "init"]) ->
        pure $ json status200 srpInit
      ("POST", ["appleauth", "auth", "signin", "complete"]) ->
        pure $
          if snSrpCompleteEmptyError scenario
            then responseLBS status401 [] ""
            else case snSrpOutcome scenario of
              SrpOk -> json status200 "{}"
              SrpNeeds2FA -> responseLBS status409 jsonHeaders "{}"
      ("GET", ["appleauth", "auth", "2sv", "trust"]) ->
        pure $ json status200 "{}"
      ("PUT", ["appleauth", "auth", "verify", "trusteddevice", "securitycode"]) ->
        pure $ responseLBS status204 [] ""
      ("POST", ["appleauth", "auth", "verify", "trusteddevice", "securitycode"]) ->
        pure $ json status200 "{}"
      ("PUT", ["appleauth", "auth", "verify", "phone"]) ->
        pure $ json status200 "{}"
      ("POST", ["appleauth", "auth", "verify", "phone", "securitycode"]) ->
        pure $ json status200 "true"
      ("GET", ["setup", "ws", "1", "listDevices"]) ->
        pure $ json status200 listDevices
      ("POST", ["setup", "ws", "1", "sendVerificationCode"]) ->
        pure $ json status200 "{}"
      ("POST", ["setup", "ws", "1", "validateVerificationCode"]) -> do
        n <- readIORef codeAttemptsRef
        writeIORef codeAttemptsRef (n + 1)
        pure $
          if snValidateCodeFails scenario && n == 0
            then responseLBS status400 [] ""
            else json status200 "{}"
      ("POST", ["setup", "ws", "1", "validate"]) ->
        pure $
          if snValidate scenario
            then json status200 "{}"
            else responseLBS status401 [] ""
      ("POST", ["setup", "ws", "1", "accountLogin"]) -> do
        n <- readIORef accountLoginRef
        writeIORef accountLoginRef (n + 1)
        pure $
          if snAccountLoginNeeds2FA scenario > n
            then json status200 login2fa
            else if snAccountLoginNeeds2SA scenario && n == 0
              then json status200 login2sa
              else json status200 loginWorking
      _ -> pure $ responseLBS status404 [] "not found"
    respond resp
