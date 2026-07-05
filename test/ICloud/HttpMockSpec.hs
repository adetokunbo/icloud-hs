{-# LANGUAGE OverloadedStrings #-}

module ICloud.HttpMockSpec (spec) where

import Data.Aeson (decode, encodeFile)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import ICloud.Mock (Scenario (..), SrpOutcome (..), defaultScenario, withMockApp)
import Network.HTTP.Client (Request (..), defaultManagerSettings, defaultRequest, newManager)
import Network.HTTP.Types (methodPost)
import Network.ICloud.Http
  ( Api
  , AuthState (..)
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
      loginShouldAuthenticate tmpDir defaultScenario

  it "returns Authenticated when saved headers are valid" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir -> do
      writeSavedHeaders tmpDir
      loginShouldAuthenticate tmpDir defaultScenario

  it "falls through to fresh login when validate returns 401" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir -> do
      writeSavedHeaders tmpDir
      loginShouldAuthenticate tmpDir defaultScenario{snValidate = False}

  it "returns Requires2FA when signin complete returns 409" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir ->
      withMockApi tmpDir defaultScenario{snSrpOutcome = SrpNeeds2FA} $ \api -> do
        result <- login api
        isRequires2FA result `shouldBe` True

  it "completeTwoFactor returns Authenticated after 2FA phone challenge" $
    withSystemTempDirectory "icloud-auth-2fa" $ \tmpDir ->
      withMockApi tmpDir defaultScenario{snSrpOutcome = SrpNeeds2FA} $ \api -> do
        loginResult <- login api
        case loginResult of
          Requires2FA _ td -> do
            result <- completeTwoFactorWith (pure "123456") api td
            isAuthenticated result `shouldBe` True
          _ -> expectationFailure "expected Requires2FA from login"

  it "complete2SA returns Authenticated after 2SA challenge" $
    withSystemTempDirectory "icloud-auth-2sa" $ \tmpDir ->
      withMockApi tmpDir defaultScenario $ \api -> do
        result <- complete2SAWith (\_ -> pure testDevice) (pure "0") api [testDevice]
        isAuthenticated result `shouldBe` True

  it "login returns Requires2SA when account login signals 2SA required" $
    withSystemTempDirectory "icloud-auth-2sa-login" $ \tmpDir ->
      withMockApi tmpDir defaultScenario{snAccountLoginNeeds2SA = True} $ \api -> do
        result <- login api
        isRequires2SA result `shouldBe` True

  it "complete2SA returns Authenticated after first-time 2SA login" $
    withSystemTempDirectory "icloud-auth-2sa-full" $ \tmpDir ->
      withMockApi tmpDir defaultScenario{snAccountLoginNeeds2SA = True} $ \api -> do
        loginResult <- login api
        case loginResult of
          Requires2SA _ devices -> do
            result <- complete2SAWith (\_ -> pure testDevice) (pure "0") api devices
            isAuthenticated result `shouldBe` True
          _ -> expectationFailure "expected Requires2SA from login"

  it "complete2SA retries when the first verification code is wrong" $
    withSystemTempDirectory "icloud-auth-2sa-retry" $ \tmpDir -> do
      codeRef <- newIORef ["wrongcode", "0"]
      let readCode = do
            codes <- readIORef codeRef
            case codes of
              [] -> fail "no more codes"
              (c : rest) -> writeIORef codeRef rest >> pure c
      withMockApi tmpDir defaultScenario{snValidateCodeFails = True} $ \api -> do
        result <- complete2SAWith (\_ -> pure testDevice) readCode api [testDevice]
        isAuthenticated result `shouldBe` True


withMockApi :: FilePath -> Scenario -> (Api -> IO a) -> IO a
withMockApi tmpDir scenario action =
  withMockApp scenario $ \serverPort -> do
    mgr <- newManager defaultManagerSettings
    api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
    action api


loginShouldAuthenticate :: FilePath -> Scenario -> IO ()
loginShouldAuthenticate tmpDir scenario =
  withMockApi tmpDir scenario $ \api -> do
    result <- login api
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


isRequires2SA :: AuthState -> Bool
isRequires2SA (Requires2SA _ _) = True
isRequires2SA _ = False


testDevice :: Setup2SADevice
testDevice =
  fromJust $
    decode
      "{\"deviceType\":\"SMS\",\"areaCode\":\"\",\"phoneNumber\":\"*******58\",\"deviceId\":\"1\"}"
