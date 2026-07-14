{-# LANGUAGE OverloadedStrings #-}

module ICloud.Http.HeadersSpec (spec) where

import Data.Aeson (decode, encodeFile)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (readIORef)
import Data.List (find)
import Data.Maybe (fromJust)
import ICloud.Mock (Scenario (..), defaultScenario, withMockAppCapturing)
import Network.HTTP.Client (Request (..), defaultManagerSettings, defaultRequest, newManager)
import Network.HTTP.Types (RequestHeaders, hAccept, hContentType, methodPost)
import Network.ICloud.Http (login, loginWith, mkApiWith)
import Network.ICloud.Http.Endpoints (Endpoints (..))
import Network.ICloud.Internal.Session (SavedHeaders (..), savedHeadersPath)
import Network.ICloud.Session (Credentials (..), Session (..))
import Network.ICloud.Trust (Setup2SADevice)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldSatisfy)


spec :: Spec
spec = describe "Network.ICloud.Http request headers" $ do
  it "signin/init sends Content-Type and Accept: application/json" $
    withCapturedLogin (\_ -> pure ()) $ \captured ->
      headersFor "/appleauth/auth/signin/init" captured
        `shouldSatisfy` hasJsonContentHeaders

  it "signin/complete sends Content-Type and Accept: application/json" $
    withCapturedLogin (\_ -> pure ()) $ \captured ->
      headersFor "/appleauth/auth/signin/complete" captured
        `shouldSatisfy` hasJsonContentHeaders

  it "2sv/trust sends Accept: application/json" $
    withCapturedTwoFa $ \captured ->
      headersFor "/appleauth/auth/2sv/trust" captured
        `shouldSatisfy` hasJsonAccept

  it "validate sends Content-Type and Accept: application/json" $
    withCapturedLogin writeSavedHeaders $ \captured ->
      headersFor "/setup/ws/1/validate" captured
        `shouldSatisfy` hasJsonContentHeaders

  it "accountLogin sends Content-Type and Accept: application/json" $
    withCapturedLogin (\_ -> pure ()) $ \captured ->
      headersFor "/setup/ws/1/accountLogin" captured
        `shouldSatisfy` hasJsonContentHeaders

  it "accountLogin sends Origin and X-Apple-Widget-Key" $
    withCapturedLogin (\_ -> pure ()) $ \captured ->
      headersFor "/setup/ws/1/accountLogin" captured
        `shouldSatisfy` (\hs -> hasOrigin hs && hasWidgetKey hs)

  it "signin/init sends X-Apple-Widget-Key" $
    withCapturedLogin (\_ -> pure ()) $ \captured ->
      headersFor "/appleauth/auth/signin/init" captured
        `shouldSatisfy` hasWidgetKey

  it "2sv/trust sends X-Apple-Widget-Key" $
    withCapturedTwoFa $ \captured ->
      headersFor "/appleauth/auth/2sv/trust" captured
        `shouldSatisfy` hasWidgetKey

  it "verify/trusteddevice/securitycode sends Content-Type, Accept: application/json, and X-Apple-Widget-Key" $
    withCapturedTwoFa $ \captured ->
      headersFor "/appleauth/auth/verify/trusteddevice/securitycode" captured
        `shouldSatisfy` (\hs -> hasJsonContentHeaders hs && hasWidgetKey hs)

  it "listDevices sends Accept: application/json and X-Apple-Widget-Key" $
    withCapturedTwoSa $ \captured ->
      headersFor "/setup/ws/1/listDevices" captured
        `shouldSatisfy` (\hs -> hasJsonAccept hs && hasWidgetKey hs)

  it "sendVerificationCode sends Content-Type, Accept: application/json, and X-Apple-Widget-Key" $
    withCapturedTwoSa $ \captured ->
      headersFor "/setup/ws/1/sendVerificationCode" captured
        `shouldSatisfy` (\hs -> hasJsonContentHeaders hs && hasWidgetKey hs)

  it "validateVerificationCode sends Content-Type, Accept: application/json, and X-Apple-Widget-Key" $
    withCapturedTwoSa $ \captured ->
      headersFor "/setup/ws/1/validateVerificationCode" captured
        `shouldSatisfy` (\hs -> hasJsonContentHeaders hs && hasWidgetKey hs)


withCapturedLogin :: (FilePath -> IO ()) -> ([(ByteString, RequestHeaders)] -> IO ()) -> IO ()
withCapturedLogin setup action =
  withSystemTempDirectory "icloud-auth-headers" $ \tmpDir -> do
    setup tmpDir
    withMockAppCapturing defaultScenario $ \serverPort capturedRef -> do
      mgr <- newManager defaultManagerSettings
      api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
      _ <- login api
      captured <- readIORef capturedRef
      action captured


withCapturedTwoFa :: ([(ByteString, RequestHeaders)] -> IO ()) -> IO ()
withCapturedTwoFa action =
  withSystemTempDirectory "icloud-auth-headers-2fa" $ \tmpDir ->
    withMockAppCapturing defaultScenario{snAccountLoginNeeds2FA = 1} $ \serverPort capturedRef -> do
      mgr <- newManager defaultManagerSettings
      api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
      _ <- loginWith (pure "123456") (\_ -> pure testDevice) api
      captured <- readIORef capturedRef
      action captured


withCapturedTwoSa :: ([(ByteString, RequestHeaders)] -> IO ()) -> IO ()
withCapturedTwoSa action =
  withSystemTempDirectory "icloud-auth-headers-2sa" $ \tmpDir ->
    withMockAppCapturing defaultScenario{snAccountLoginNeeds2SA = True} $ \serverPort capturedRef -> do
      mgr <- newManager defaultManagerSettings
      api <- mkApiWith (testSession tmpDir) (testEndpoints serverPort) mgr
      _ <- loginWith (pure "0") (\_ -> pure testDevice) api
      captured <- readIORef capturedRef
      action captured


writeSavedHeaders :: FilePath -> IO ()
writeSavedHeaders tmpDir = do
  let creds = Credentials "alice@example.com" "password123"
      hdrs = SavedHeaders Nothing Nothing Nothing (Just "test-token") Nothing Nothing
  encodeFile (savedHeadersPath tmpDir creds) hdrs


headersFor :: ByteString -> [(ByteString, RequestHeaders)] -> Maybe RequestHeaders
headersFor pathFragment captured =
  fmap snd $ find (\(p, _) -> pathFragment `BS8.isSuffixOf` p) captured


hasJsonAccept :: Maybe RequestHeaders -> Bool
hasJsonAccept Nothing = False
hasJsonAccept (Just hs) = any (\(n, v) -> n == hAccept && v == "application/json") hs


hasJsonContentType :: Maybe RequestHeaders -> Bool
hasJsonContentType Nothing = False
hasJsonContentType (Just hs) = any (\(n, v) -> n == hContentType && v == "application/json") hs


hasWidgetKey :: Maybe RequestHeaders -> Bool
hasWidgetKey Nothing = False
hasWidgetKey (Just hs) = any (\(n, _) -> n == "X-Apple-Widget-Key") hs


hasOrigin :: Maybe RequestHeaders -> Bool
hasOrigin Nothing = False
hasOrigin (Just hs) = any (\(n, _) -> n == "Origin") hs


hasJsonContentHeaders :: Maybe RequestHeaders -> Bool
hasJsonContentHeaders hs = hasJsonAccept hs && hasJsonContentType hs


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


testDevice :: Setup2SADevice
testDevice =
  fromJust $
    decode
      "{\"deviceType\":\"SMS\",\"areaCode\":\"\",\"phoneNumber\":\"*******58\",\"deviceId\":\"1\"}"
