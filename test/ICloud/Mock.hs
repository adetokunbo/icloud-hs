{-# LANGUAGE OverloadedStrings #-}

module ICloud.Mock
  ( Scenario (..)
  , SrpOutcome (..)
  , withMockApp
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types
  ( HeaderName
  , hContentType
  , status200
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
  { scenValidate :: Bool
  , scenSrpOutcome :: SrpOutcome
  }


jsonHeaders :: [(HeaderName, ByteString)]
jsonHeaders = [(hContentType, "application/json")]


withMockApp :: Scenario -> (Int -> IO a) -> IO a
withMockApp scenario action = do
  srpInit <- LBS.readFile =<< getDataFileName "testdata/srp_init_ok_test.json"
  trustData <- LBS.readFile =<< getDataFileName "testdata/trust_data_test.json"
  loginWorking <- LBS.readFile =<< getDataFileName "testdata/login_working_test.json"
  testWithApplication (pure $ mockApp scenario srpInit trustData loginWorking) action


mockApp
  :: Scenario
  -> LBS.ByteString
  -> LBS.ByteString
  -> LBS.ByteString
  -> Application
mockApp scenario srpInit trustData loginWorking req respond = do
  let method = requestMethod req
      segs = pathInfo req
      json st body = responseLBS st jsonHeaders body
      resp = case (method, segs) of
        ("POST", ["appleauth", "auth", "signin", "init"]) ->
          json status200 srpInit
        ("POST", ["appleauth", "auth", "signin", "complete"]) ->
          case scenSrpOutcome scenario of
            SrpOk -> json status200 "{}"
            SrpNeeds2FA -> json status409 "{}"
        ("GET", ["appleauth", "auth", "2sv", "trust"]) ->
          json status200 "{}"
        ("POST", ["appleauth", "auth"]) ->
          json status200 trustData
        ("POST", ["setup", "ws", "1", "validate"]) ->
          if scenValidate scenario
            then json status200 "{}"
            else responseLBS status401 [] ""
        ("POST", ["setup", "ws", "1", "accountLogin"]) ->
          json status200 loginWorking
        _ -> responseLBS status404 [] "not found"
  respond resp
