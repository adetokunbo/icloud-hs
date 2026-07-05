{-# LANGUAGE OverloadedStrings #-}

module ICloud.HttpMockSpec (spec) where

import Data.Aeson (encodeFile)
import qualified Data.ByteString.Char8 as BS8
import ICloud.Mock (Scenario (..), SrpOutcome (..), withMockApp)
import Network.HTTP.Client (Request (..), defaultManagerSettings, defaultRequest, newManager)
import Network.HTTP.Types (methodPost)
import Network.ICloud.Http (AuthState (..), login, mkApiWith)
import Network.ICloud.Http.Endpoints (Endpoints (..))
import Network.ICloud.Session
  ( Credentials (..)
  , SavedHeaders (..)
  , Session (..)
  , savedHeadersPath
  )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)


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
