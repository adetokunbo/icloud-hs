{-# LANGUAGE OverloadedStrings #-}

module ICloud.Drive.MutationSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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
  describe "createFolder" $ do
    it "returns () on success" $
      withMock createOkApp $ \ep api ->
        createFolder api ep rootNodeId "New Folder" `shouldReturn` ()
    it "raises an error on HTTP failure" $
      withMock errorApp $ \ep api ->
        createFolder api ep rootNodeId "New Folder" `shouldThrow` anyException

  describe "renameNode" $ do
    it "returns () when renaming a folder" $
      withMock renameOkApp $ \ep api ->
        renameNode api ep testFolderNode "Renamed Folder" `shouldReturn` ()
    it "returns () when renaming a file" $
      withMock renameOkApp $ \ep api ->
        renameNode api ep testFileNode "Renamed File" `shouldReturn` ()
    it "raises an error on HTTP failure" $
      withMock errorApp $ \ep api ->
        renameNode api ep testFolderNode "Renamed Folder" `shouldThrow` anyException

  describe "deleteNode" $ do
    it "returns () when deleting a folder" $
      withMock deleteOkApp $ \ep api ->
        deleteNode api ep testFolderNode `shouldReturn` ()
    it "returns () when deleting a file" $
      withMock deleteOkApp $ \ep api ->
        deleteNode api ep testFileNode `shouldReturn` ()
    it "raises an error on HTTP failure" $
      withMock errorApp $ \ep api ->
        deleteNode api ep testFolderNode `shouldThrow` anyException


-- Mock servers

withMock :: Application -> (DriveEndpoints -> Api -> IO a) -> IO a
withMock app action =
  withSystemTempDirectory "icloud-drive-mutation" $ \tmpDir ->
    testWithApplication (pure app) $ \serverPort -> do
      (ep, api) <- mkEpAndApi serverPort tmpDir
      action ep api


createOkApp :: Application
createOkApp req respond
  | "/createFolders" `BS.isSuffixOf` rawPathInfo req =
      respond $ responseLBS status200 jsonHeaders "{}"
  | otherwise =
      respond $ responseLBS status400 [] "unexpected path"


renameOkApp :: Application
renameOkApp req respond
  | "/renameItems" `BS.isSuffixOf` rawPathInfo req =
      respond $ responseLBS status200 jsonHeaders "{}"
  | otherwise =
      respond $ responseLBS status400 [] "unexpected path"


deleteOkApp :: Application
deleteOkApp req respond
  | "/moveItemsToTrash" `BS.isSuffixOf` rawPathInfo req =
      respond $ responseLBS status200 jsonHeaders "{}"
  | otherwise =
      respond $ responseLBS status400 [] "unexpected path"


errorApp :: Application
errorApp _req respond = respond $ responseLBS status400 [] "bad request"


mkEpAndApi :: Int -> FilePath -> IO (DriveEndpoints, Api)
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


testFolderNode :: DriveNode
testFolderNode =
  DriveFolder
    FolderData
      { fnId = DriveNodeId "FOLDER::com.apple.CloudDocs::test-folder"
      , fnEtag = "1a"
      , fnName = "Test Folder"
      , fnZone = "com.apple.CloudDocs"
      , fnDateCreated = Nothing
      }


testFileNode :: DriveNode
testFileNode =
  DriveFile
    FileData
      { fdId = DriveNodeId "FILE::com.apple.CloudDocs::test-file"
      , fdDocId = "test-file-doc-id"
      , fdEtag = "2b"
      , fdName = "Test File"
      , fdExtension = Just "txt"
      , fdZone = "com.apple.CloudDocs"
      , fdSize = Just 100
      , fdDateCreated = Nothing
      , fdDateModified = Nothing
      }


jsonHeaders :: [(HeaderName, BS8.ByteString)]
jsonHeaders = [(hContentType, "application/json")]
