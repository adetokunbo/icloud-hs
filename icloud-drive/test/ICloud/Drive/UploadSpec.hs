{-# LANGUAGE OverloadedStrings #-}

module ICloud.Drive.UploadSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Network.HTTP.Client
  ( Request (..)
  , defaultManagerSettings
  , defaultRequest
  , newManager
  )
import Network.HTTP.Types (HeaderName, hContentType, methodPost, status200, status400)
import Network.ICloud.Drive
import Network.ICloud.Http (Api, mkApiWith)
import Network.ICloud.Http.Endpoints (Endpoints (..))
import Network.ICloud.Session (AccountData (..), Credentials (..), Session (..))
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec


spec :: Spec
spec = describe "Network.ICloud.Drive" $ do
  describe "uploadFile" $ do
    it "returns () on success" $
      withUploadMock uploadOkApp $ \ep api ->
        uploadFile api ep testFolderData "hello.txt" "hello world" `shouldReturn` ()
    it "raises an error when the token request fails" $
      withUploadMock errorApp $ \ep api ->
        uploadFile api ep testFolderData "hello.txt" "hello world" `shouldThrow` anyException
    it "raises an error when the content upload fails" $
      withUploadMock contentErrorApp $ \ep api ->
        uploadFile api ep testFolderData "hello.txt" "hello world" `shouldThrow` anyException
    it "raises an error when the commit request fails" $
      withUploadMock commitErrorApp $ \ep api ->
        uploadFile api ep testFolderData "hello.txt" "hello world" `shouldThrow` anyException


-- Mock servers

withUploadMock :: (Int -> Application) -> (DriveEndpoints CloudScope -> Api -> IO a) -> IO a
withUploadMock mkApp action =
  withSystemTempDirectory "icloud-drive-upload" $ \tmpDir -> do
    portRef <- newIORef 0
    testWithApplication (pure (dynApp portRef mkApp)) $ \serverPort -> do
      writeIORef portRef serverPort
      (ep, api) <- mkEpAndApi serverPort tmpDir
      action ep api
 where
  dynApp portRef mk req respond = do
    serverPort <- readIORef portRef
    mk serverPort req respond


uploadOkApp :: Int -> Application
uploadOkApp serverPort req respond
  | "/upload/web" `BS.isSuffixOf` rawPathInfo req =
      let url = "http://127.0.0.1:" ++ show serverPort ++ "/upload/content"
          body = LBS8.pack $ "[{\"document_id\":\"test-doc-id\",\"url\":\"" ++ url ++ "\"}]"
       in respond $ responseLBS status200 jsonHeaders body
  | rawPathInfo req == "/upload/content" =
      respond $ responseLBS status200 jsonHeaders receiptJson
  | "/update/documents" `BS.isSuffixOf` rawPathInfo req =
      respond $ responseLBS status200 jsonHeaders "{}"
  | otherwise =
      respond $ responseLBS status400 [] "unexpected path"


errorApp :: Int -> Application
errorApp _port _req respond = respond $ responseLBS status400 [] "bad request"


contentErrorApp :: Int -> Application
contentErrorApp _port req respond
  | "/upload/web" `BS.isSuffixOf` rawPathInfo req =
      respond $ responseLBS status200 jsonHeaders badTokenJson
  | otherwise =
      respond $ responseLBS status400 [] "bad request"
 where
  badTokenJson = "[{\"document_id\":\"x\",\"url\":\"http://127.0.0.1:1/no-such\"}]"


commitErrorApp :: Int -> Application
commitErrorApp serverPort req respond
  | "/upload/web" `BS.isSuffixOf` rawPathInfo req =
      let url = "http://127.0.0.1:" ++ show serverPort ++ "/upload/content"
          body = LBS8.pack $ "[{\"document_id\":\"test-doc-id\",\"url\":\"" ++ url ++ "\"}]"
       in respond $ responseLBS status200 jsonHeaders body
  | rawPathInfo req == "/upload/content" =
      respond $ responseLBS status200 jsonHeaders receiptJson
  | otherwise =
      respond $ responseLBS status400 [] "bad request"


mkEpAndApi :: Int -> FilePath -> IO (DriveEndpoints CloudScope, Api)
mkEpAndApi serverPort tmpDir = do
  let baseUrl = Text.pack $ "http://127.0.0.1:" ++ show serverPort
  ep <- mkDriveEndpoints (testAccountData baseUrl) (testSession tmpDir)
  mgr <- newManager defaultManagerSettings
  api <- mkApiWith (testSession tmpDir) (testAuthEndpoints serverPort) mgr
  pure (ep, api)


-- Fixtures

testAccountData :: Text.Text -> AccountData
testAccountData baseUrl =
  AccountData
    { adHsaVersion = 2
    , adHsaChallengeRequired = False
    , adHsaTrustedBrowser = True
    , adWebservices = Map.fromList [("drivews", baseUrl), ("docws", baseUrl)]
    }


testSession :: FilePath -> Session
testSession topDir =
  Session
    { sessionCreds = Credentials{credAccountName = "test@example.com", credPassword = "test-pass"}
    , sessionTopDir = topDir
    , sessionClientId = "auth-test-client-id"
    }


testAuthEndpoints :: Int -> Endpoints
testAuthEndpoints serverPort =
  Endpoints
    { epHome = "http://127.0.0.1:" <> BS8.pack (show serverPort)
    , epAuth = dummyReq "/appleauth/auth"
    , epSetup = dummyReq "/setup/ws/1"
    }
 where
  dummyReq reqPath =
    defaultRequest
      { host = "127.0.0.1"
      , port = serverPort
      , secure = False
      , method = methodPost
      , path = reqPath
      }


testFolderData :: FolderData
testFolderData =
  FolderData
    { fnId = DriveNodeId "FOLDER::com.apple.CloudDocs::test-folder-doc"
    , fnEtag = "1a"
    , fnName = "Test Folder"
    , fnZone = "com.apple.CloudDocs"
    , fnDateCreated = Nothing
    }


receiptJson :: LBS.ByteString
receiptJson =
  "{\"singleFile\":\
  \{\"fileChecksum\":\"chk\"\
  \,\"wrappingKey\":\"wk\"\
  \,\"referenceChecksum\":\"rc\"\
  \,\"size\":11\
  \,\"receipt\":\"rcpt\"\
  \}}"


jsonHeaders :: [(HeaderName, BS8.ByteString)]
jsonHeaders = [(hContentType, "application/json")]
