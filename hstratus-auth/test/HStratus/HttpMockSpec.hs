{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use lambda-case" -}

module HStratus.HttpMockSpec (spec) where

import Control.Exception (try)
import Data.Aeson (decode, encodeFile)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import HStratus.Mock (Scenario (..), SrpOutcome (..), defaultScenario, withMockApp, withMockAppCapturing)
import Network.HStratus.Http
  ( Api
  , AuthState (..)
  , complete2SAWith
  , login
  , loginWith
  , mkApiWith
  )
import Network.HStratus.Http.Endpoints (Endpoints (..))
import Network.HStratus.Internal.HttpErrors (AuthError (..))
import Network.HStratus.Internal.Session (SavedHeaders (..), savedHeadersPath)
import Network.HStratus.Session (Credentials (..), Session (..))
import Network.HStratus.Trust (Setup2SADevice, TrustedPhone (..))
import Network.HTTP.Client (Request (..), defaultManagerSettings, defaultRequest, newManager)
import Network.HTTP.Types (methodPost)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)


spec :: Spec
spec = describe "Network.HStratus.Http.login" $ do
  it "returns Authenticated on fresh login" $
    loginShouldAuthenticate defaultScenario

  it "creates the session directory when absent then returns Authenticated" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir ->
      withMockApi (tmpDir </> "session") defaultScenario $ \api -> do
        isAuthenticated <$> login api `shouldReturn` True

  it "returns Authenticated when saved headers are valid" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir -> do
      writeSavedHeaders tmpDir
      withMockApi tmpDir defaultScenario $ \api -> do
        isAuthenticated <$> login api `shouldReturn` True

  it "falls through to fresh login when validate returns 401" $
    withSystemTempDirectory "icloud-auth-mock" $ \tmpDir -> do
      writeSavedHeaders tmpDir
      withMockApi tmpDir defaultScenario{snValidate = False} $ \api -> do
        isAuthenticated <$> login api `shouldReturn` True

  it "completes 2FA automatically when accountLogin requires 2FA" $
    withFreshMockApi "icloud-auth-mock" defaultScenario{snAccountLoginNeeds2FA = 1} $ \api -> do
      isAuthenticated <$> loginWith (\_ -> pure "123456") (\_ -> pure Nothing) (\_ -> pure testDevice) api `shouldReturn` True

  it "complete2SA returns Authenticated after 2SA challenge" $
    withFreshMockApi "icloud-auth-2sa" defaultScenario $ \api -> do
      result <- complete2SAWith (\_ -> pure testDevice) (pure "0") api (testDevice :| [])
      isAuthenticated result `shouldBe` True

  it "completes 2SA automatically when account login signals 2SA required" $
    withFreshMockApi "icloud-auth-2sa-login" defaultScenario{snAccountLoginNeeds2SA = True} $ \api -> do
      isAuthenticated <$> loginWith (\_ -> pure "0") (\_ -> pure Nothing) (\_ -> pure testDevice) api `shouldReturn` True

  it "throws UnexpectedResponse with HTTP status when error response has no body" $
    withFreshMockApi "icloud-auth-empty-err" defaultScenario{snSrpCompleteEmptyError = True} $ \api -> do
      result <- try (login api) :: IO (Either AuthError AuthState)
      result
        `shouldSatisfy` ( \r -> case r of
                            Left (UnexpectedResponse msg) -> "bad request" `Text.isPrefixOf` msg
                            _ -> False
                        )

  it "complete2SA retries when the first verification code is wrong" $ do
    codeRef <- newIORef ["wrongcode", "0"]
    let readCode = do
          codes <- readIORef codeRef
          case codes of
            [] -> fail "no more codes"
            (c : rest) -> writeIORef codeRef rest >> pure c
    withFreshMockApi "icloud-auth-2sa-retry" defaultScenario{snValidateCodeFails = True} $ \api -> do
      result <- complete2SAWith (\_ -> pure testDevice) readCode api (testDevice :| [])
      isAuthenticated result `shouldBe` True

  it "completes 2FA via SMS when phone selector returns a phone" $
    withFreshMockApi "icloud-auth-2fa-sms" defaultScenario{snAccountLoginNeeds2FA = 1} $ \api -> do
      isAuthenticated <$> loginWith (\_ -> pure "654321") (\_ -> pure (Just testPhone)) (\_ -> pure testDevice) api
        `shouldReturn` True

  it "calls GET /appleauth/auth when accountLogin requires 2FA" $
    withSystemTempDirectory "icloud-auth-fetches-trust" $ \tmpDir -> do
      let scenario = defaultScenario{snSrpOutcome = SrpNeeds2FA, snAccountLoginNeeds2FA = 1}
      withMockAppCapturing scenario $ \serverPort capturedRef -> do
        mgr <- newManager defaultManagerSettings
        api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
        _ <- loginWith (\_ -> pure "123456") (\_ -> pure Nothing) (\_ -> pure testDevice) api
        captured <- readIORef capturedRef
        map fst captured `shouldSatisfy` elem "/appleauth/auth"

  it "completes 2FA via device push after retrying when the first code is wrong" $ do
    codeRef <- newIORef ["wrongcode", "123456"]
    let readCode = do
          codes <- readIORef codeRef
          case codes of
            [] -> fail "no more codes"
            (c : rest) -> writeIORef codeRef rest >> pure c
    withFreshMockApi "icloud-auth-2fa-retry" defaultScenario{snAccountLoginNeeds2FA = 1, snVerifyDeviceCodeFails = True} $ \api ->
      isAuthenticated <$> loginWith (\_ -> readCode) (\_ -> pure Nothing) (\_ -> pure testDevice) api
        `shouldReturn` True

  it "retries signin/init and completes login when first response is 421" $
    loginShouldAuthenticate defaultScenario{snSrpInitReturnsRetryCode = Just 421}

  it "retries signin/init and completes login when first response is 450" $
    loginShouldAuthenticate defaultScenario{snSrpInitReturnsRetryCode = Just 450}

  it "retries signin/init and completes login when first response is 500" $
    loginShouldAuthenticate defaultScenario{snSrpInitReturnsRetryCode = Just 500}

  it "throws TwoFactorLocked when the server signals the account is locked" $
    withFreshMockApi "icloud-auth-2fa-locked" defaultScenario{snAccountLoginNeeds2FA = 1, snVerifyCodeLocks = True} $ \api -> do
      result <- try (loginWith (\_ -> pure "wrongcode") (\_ -> pure Nothing) (\_ -> pure testDevice) api) :: IO (Either AuthError AuthState)
      result `shouldSatisfy` (\r -> case r of Left TwoFactorLocked -> True; _ -> False)


withMockApi :: FilePath -> Scenario -> (Api -> IO a) -> IO a
withMockApi tmpDir scenario action =
  withMockApp scenario $ \serverPort -> do
    mgr <- newManager defaultManagerSettings
    api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
    action api


withFreshMockApi :: String -> Scenario -> (Api -> IO a) -> IO a
withFreshMockApi prefix scenario action =
  withSystemTempDirectory prefix $ \tmpDir ->
    withMockApi tmpDir scenario action


loginShouldAuthenticate :: Scenario -> IO ()
loginShouldAuthenticate scenario =
  withFreshMockApi "icloud-auth-mock" scenario $ \api -> do
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
    , epWidgetKey = "test-widget-key"
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


testDevice :: Setup2SADevice
testDevice =
  fromJust $
    decode
      "{\"deviceType\":\"SMS\",\"areaCode\":\"\",\"phoneNumber\":\"*******58\",\"deviceId\":\"1\"}"


testPhone :: TrustedPhone
testPhone = TrustedPhone 1 "+81 test" (Just "sms")
