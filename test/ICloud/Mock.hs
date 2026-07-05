{-# LANGUAGE OverloadedStrings #-}

module ICloud.Mock
  ( Scenario (..)
  , SrpOutcome (..)
  , defaultScenario
  , withMockApp
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.HTTP.Types
  ( HeaderName
  , hContentType
  , status200
  , status400
  , status401
  , status404
  , status409
  )
import Network.Wai (Application, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Paths_icloud_auth (getDataFileName)


data SrpOutcome = SrpOk | SrpNeeds2FA
  deriving (Eq, Show)


data Scenario = Scenario
  { snValidate :: Bool
  , snSrpOutcome :: SrpOutcome
  , snValidateCodeFails :: Bool
  -- ^ when True, the first validateVerificationCode call returns 400
  }


defaultScenario :: Scenario
defaultScenario =
  Scenario
    { snValidate = True
    , snSrpOutcome = SrpOk
    , snValidateCodeFails = False
    }


jsonHeaders :: [(HeaderName, ByteString)]
jsonHeaders = [(hContentType, "application/json")]


withMockApp :: Scenario -> (Int -> IO a) -> IO a
withMockApp scenario action = do
  srpInit <- LBS.readFile =<< getDataFileName "testdata/srp_init_ok_test.json"
  trustData <- LBS.readFile =<< getDataFileName "testdata/trust_data_test.json"
  loginWorking <- LBS.readFile =<< getDataFileName "testdata/login_working_test.json"
  listDevices <- LBS.readFile =<< getDataFileName "testdata/trusted_devices_test.json"
  codeAttemptsRef <- newIORef (0 :: Int)
  testWithApplication
    (pure $ mockApp scenario srpInit trustData loginWorking listDevices codeAttemptsRef)
    action


mockApp
  :: Scenario
  -> LBS.ByteString
  -> LBS.ByteString
  -> LBS.ByteString
  -> LBS.ByteString
  -> IORef Int
  -> Application
mockApp scenario srpInit trustData loginWorking listDevices codeAttemptsRef req respond = do
  let method = requestMethod req
      segs = pathInfo req
      json st body = responseLBS st jsonHeaders body
  resp <- case (method, segs) of
    ("POST", ["appleauth", "auth", "signin", "init"]) ->
      pure $ json status200 srpInit
    ("POST", ["appleauth", "auth", "signin", "complete"]) ->
      pure $ case snSrpOutcome scenario of
        SrpOk -> json status200 "{}"
        SrpNeeds2FA -> json status409 "{}"
    ("GET", ["appleauth", "auth", "2sv", "trust"]) ->
      pure $ json status200 "{}"
    ("POST", ["appleauth", "auth"]) ->
      pure $ json status200 trustData
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
    ("POST", ["setup", "ws", "1", "accountLogin"]) ->
      pure $ json status200 loginWorking
    _ -> pure $ responseLBS status404 [] "not found"
  respond resp
