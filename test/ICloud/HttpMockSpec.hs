{-# LANGUAGE OverloadedStrings #-}

module ICloud.HttpMockSpec (spec) where

import Data.Aeson (decode, encodeFile)
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromJust)
import ICloud.Mock (Scenario (..), SrpOutcome (..), withMockApp)
import Network.HTTP.Client (Request (..), defaultManagerSettings, defaultRequest, newManager)
import Network.HTTP.Types (methodPost)
import Network.ICloud.Http
  ( AuthState (..)
  , complete2SAWith
  , completeTwoFactorWith
  , login
  , mkApiWith
  )
import Network.ICloud.Http.Endpoints (Endpoints (..))
import Network.ICloud.Session
  ( Credentials (..)
  , SavedHeaders (..)
  , Session (..)
  , savedHeadersPath
  )
import Network.ICloud.Trust (Setup2SADevice)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)


spec :: Spec
spec = describe "Network.ICloud.Http.login" $ do
  it "returns Authenticated on fresh login" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir ->
      withMockApp (Scenario True SrpOk) $ \serverPort -> do
        mgr <- newManager defaultManagerSettings
        api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
        result <- login api
        isAuthenticated result `shouldBe` True

  it "returns Authenticated when saved headers are valid" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir -> do
      writeSavedHeaders tmpDir
      withMockApp (Scenario True SrpOk) $ \serverPort -> do
        mgr <- newManager defaultManagerSettings
        api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
        result <- login api
        isAuthenticated result `shouldBe` True

  it "falls through to fresh login when validate returns 401" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir -> do
      writeSavedHeaders tmpDir
      withMockApp (Scenario False SrpOk) $ \serverPort -> do
        mgr <- newManager defaultManagerSettings
        api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
        result <- login api
        isAuthenticated result `shouldBe` True

  it "returns Requires2FA when signin complete returns 409" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir ->
      withMockApp (Scenario True SrpNeeds2FA) $ \serverPort -> do
        mgr <- newManager defaultManagerSettings
        api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
        result <- login api
        isRequires2FA result `shouldBe` True

  it "completeTwoFactor returns Authenticated after 2FA phone challenge" $
    withSystemTempDirectory "icloud-auth-2fa" $ \tmpDir ->
      withMockApp (Scenario True SrpNeeds2FA) $ \serverPort -> do
        mgr <- newManager defaultManagerSettings
        api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
        loginResult <- login api
        case loginResult of
          Requires2FA _ td -> do
            result <- completeTwoFactorWith (pure "123456") api td
            isAuthenticated result `shouldBe` True
          _ -> expectationFailure "expected Requires2FA from login"

  it "complete2SA returns Authenticated after 2SA challenge" $
    withSystemTempDirectory "icloud-auth-2sa" $ \tmpDir ->
      withMockApp (Scenario True SrpOk) $ \serverPort -> do
        mgr <- newManager defaultManagerSettings
        api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
        result <- complete2SAWith (\_ -> pure testDevice) (pure "0") api [testDevice]
        isAuthenticated result `shouldBe` True


testSession :: FilePath -> Session
testSession topDir =
  Session
    { sessionCreds = Credentials "alice@example.com" "password123"
    , sessionTopDir = topDir
    , sessionClientId = "test-client-id"
    }


testEndpoints :: Int -> Endpoints
testEndpoints serverPort =
  Endpoints
    { epHome = "http://127.0.0.1:" <> BS8.pack (show serverPort)
    , epAuth = mockReq "/appleauth/auth"
    , epSetup = mockReq "/setup/ws/1"
    }
 where
  mockReq reqPath =
    defaultRequest
      { host = "127.0.0.1"
      , port = serverPort
      , secure = False
      , method = methodPost
      , path = reqPath
      }


writeSavedHeaders :: FilePath -> IO ()
writeSavedHeaders tmpDir = do
  let creds = Credentials "alice@example.com" "password123"
      hdrsPath = savedHeadersPath tmpDir creds
      hdrs = SavedHeaders Nothing Nothing (Just "test-token") Nothing Nothing
  encodeFile hdrsPath hdrs


isAuthenticated :: AuthState -> Bool
isAuthenticated (Authenticated _ _) = True
isAuthenticated _ = False


isRequires2FA :: AuthState -> Bool
isRequires2FA (Requires2FA _ _) = True
isRequires2FA _ = False


testDevice :: Setup2SADevice
testDevice =
  fromJust $
    decode
      "{\"deviceType\":\"SMS\",\"areaCode\":\"\",\"phoneNumber\":\"*******58\",\"deviceId\":\"1\"}"
