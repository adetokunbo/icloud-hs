{-# LANGUAGE OverloadedStrings #-}

module HStratus.DriveSpec (spec) where

import Control.Exception (displayException)
import Data.Aeson (object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Network.HStratus.Drive
import Network.HStratus.Http (mkApiWith)
import Network.HStratus.Http.Endpoints (Endpoints (..))
import Network.HStratus.Session (AccountData (..), Credentials (..), Session (..), Webservice (..))
import Network.HTTP.Client (Request (..), defaultManagerSettings, defaultRequest, newManager)
import Network.HTTP.Types (HeaderName, hContentType, methodPost, status200, status404)
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec


spec :: Spec
spec = describe "Network.HStratus.Drive" $ do
  describe "DriveError displayException" $ do
    it "DriveHttpError" $
      displayException (DriveHttpError 404) `shouldBe` "iCloud Drive: HTTP error 404"
    it "DriveParseError" $
      displayException (DriveParseError "bad json") `shouldBe` "iCloud Drive: parse error: bad json"
    it "DriveInvalidRoot" $
      displayException DriveInvalidRoot `shouldBe` "iCloud Drive: invalid root node"

  describe "driveRoot" $ do
    it "returns root FolderData" $
      withNodeMock rootJson $ \da -> do
        fd <- driveRoot da
        fnId fd `shouldBe` DriveNodeId "FOLDER::com.apple.CloudDocs::root"

  describe "listFolder" $ do
    it "returns all children" $
      withNodeMock subfolderJson $ \da -> do
        nodes <- listFolder da (DriveNodeId "FOLDER::com.apple.CloudDocs::D5AA0425")
        length nodes `shouldBe` 2
    it "returns DriveFile nodes for file children" $
      withNodeMock subfolderJson $ \da -> do
        nodes <- listFolder da (DriveNodeId "FOLDER::com.apple.CloudDocs::D5AA0425")
        all isFile nodes `shouldBe` True

  describe "downloadFile" $ do
    it "returns LBS.empty when size is absent" $
      withNodeMock rootJson $ \da -> do
        let fd = testFileData{fdSize = Nothing}
        downloadFile da fd `shouldReturn` LBS.empty
    it "returns LBS.empty when size is zero" $
      withNodeMock rootJson $ \da -> do
        let fd = testFileData{fdSize = Just 0}
        downloadFile da fd `shouldReturn` LBS.empty
    it "downloads file contents" $
      withDownloadMock $ \da ->
        downloadFile da testFileData `shouldReturn` "test file content"


-- Mock servers

withNodeMock :: LBS.ByteString -> (DriveApi -> IO a) -> IO a
withNodeMock nodeJson action =
  withSystemTempDirectory "icloud-drive-mock" $ \tmpDir ->
    testWithApplication (pure (nodeApp nodeJson)) $ \serverPort -> do
      da <- mkEpAndApi serverPort tmpDir
      action da


withDownloadMock :: (DriveApi -> IO a) -> IO a
withDownloadMock action =
  withSystemTempDirectory "icloud-drive-download" $ \tmpDir -> do
    portRef <- newIORef 0
    testWithApplication (pure (downloadApp portRef)) $ \serverPort -> do
      writeIORef portRef serverPort
      da <- mkEpAndApi serverPort tmpDir
      action da


nodeApp :: LBS.ByteString -> Application
nodeApp nodeJson req respond
  | "/retrieveItemDetailsInFolders" `BS.isSuffixOf` rawPathInfo req =
      respond $ responseLBS status200 jsonHeaders nodeJson
  | otherwise =
      respond $ responseLBS status404 [] "not found"


downloadApp :: IORef Int -> Application
downloadApp portRef req respond = do
  serverPort <- readIORef portRef
  let p = rawPathInfo req
  if "/download/by_id" `BS.isSuffixOf` p
    then
      let url = "http://127.0.0.1:" ++ show serverPort ++ "/content/test"
          body = LBS8.pack $ "{\"data_token\":{\"url\":\"" ++ url ++ "\",\"token\":\"tok\"}}"
       in respond $ responseLBS status200 jsonHeaders body
    else
      if p == "/content/test"
        then respond $ responseLBS status200 [] "test file content"
        else respond $ responseLBS status404 [] "not found"


mkEpAndApi :: Int -> FilePath -> IO DriveApi
mkEpAndApi serverPort tmpDir = do
  let baseUrl = Text.pack $ "http://127.0.0.1:" ++ show serverPort
  mgr <- newManager defaultManagerSettings
  api <- mkApiWith (testSession tmpDir) (testAuthEndpoints serverPort) mgr
  mkDriveApi (testAccountData baseUrl) (testSession tmpDir) api


-- Fixtures

testAccountData :: Text.Text -> AccountData
testAccountData baseUrl =
  AccountData
    { adHsaVersion = 2
    , adHsaChallengeRequired = False
    , adHsaTrustedBrowser = True
    , adWebservices = Map.fromList [("drivews", Webservice baseUrl Nothing), ("docws", Webservice baseUrl Nothing)]
    , adRaw = object []
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
    , epWidgetKey = "test-widget-key"
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


testFileData :: FileData
testFileData =
  FileData
    { fdId = DriveNodeId "FILE::com.apple.CloudDocs::33A41112"
    , fdDocId = "33A41112"
    , fdEtag = "2k::2j"
    , fdName = "Scan 2"
    , fdExtension = Just "pdf"
    , fdZone = "com.apple.CloudDocs"
    , fdSize = Just 19876991
    , fdDateCreated = Nothing
    , fdDateModified = Nothing
    }


isFile :: DriveNode -> Bool
isFile (DriveFile _) = True
isFile _ = False


jsonHeaders :: [(HeaderName, BS8.ByteString)]
jsonHeaders = [(hContentType, "application/json")]


rootJson :: LBS.ByteString
rootJson =
  "[{\"drivewsid\":\"FOLDER::com.apple.CloudDocs::root\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"\"\
  \,\"etag\":\"31\"\
  \,\"type\":\"FOLDER\"\
  \}]"


subfolderJson :: LBS.ByteString
subfolderJson =
  "[{\"drivewsid\":\"FOLDER::com.apple.CloudDocs::D5AA0425-E84F-4501-AF5D-60F1D92648CF\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"Test\"\
  \,\"etag\":\"2z\"\
  \,\"type\":\"FOLDER\"\
  \,\"items\":[\
  \{\"drivewsid\":\"FILE::com.apple.CloudDocs::33A41112-4131-4938-9691-7F356CE3C51D\"\
  \,\"docwsid\":\"33A41112-4131-4938-9691-7F356CE3C51D\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"Scan 2\"\
  \,\"dateModified\":\"2020-04-27T21:37:36Z\"\
  \,\"size\":19876991\
  \,\"etag\":\"2k::2j\"\
  \,\"extension\":\"pdf\"\
  \,\"type\":\"FILE\"\
  \}\
  \,{\"drivewsid\":\"FILE::com.apple.CloudDocs::516C896C-6AA5-4A30-B30E-5502C2333DAE\"\
  \,\"docwsid\":\"516C896C-6AA5-4A30-B30E-5502C2333DAE\"\
  \,\"zone\":\"com.apple.CloudDocs\"\
  \,\"name\":\"Scanned document 1\"\
  \,\"dateModified\":\"2020-05-03T00:15:17Z\"\
  \,\"size\":21644358\
  \,\"etag\":\"32::2x\"\
  \,\"extension\":\"pdf\"\
  \,\"type\":\"FILE\"\
  \}\
  \]}]"
