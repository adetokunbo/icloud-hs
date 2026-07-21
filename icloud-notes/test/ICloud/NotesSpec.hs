{-# LANGUAGE OverloadedStrings #-}

module ICloud.NotesSpec (spec) where

import Data.Aeson (object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Network.HTTP.Client (Request (..), defaultManagerSettings, defaultRequest, newManager)
import Network.HTTP.Types (HeaderName, hContentType, methodPost, status200, status404)
import Network.ICloud.Http (Api, mkApiWith)
import Network.ICloud.Http.Endpoints (Endpoints (..))
import Network.ICloud.Internal.Notes.Download
  ( fetchFolders
  , fetchNote
  , fetchNotesInFolder
  , fetchRecent
  )
import Network.ICloud.Internal.Notes.Endpoints (NotesEndpoints, mkNotesEndpoints)
import Network.ICloud.Notes.Note
import Network.ICloud.Session (AccountData (..), Credentials (..), Session (..))
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec


spec :: Spec
spec = describe "Network.ICloud.Notes" $ do
  describe "fetchFolders" $ do
    it "returns NoteFolder list from a query response" $
      withNotesMock queryFoldersJson "/records/query" $ \ep api -> do
        folders <- fetchFolders api ep
        case folders of
          [f] -> do
            nfId f `shouldBe` FolderId "Folder/FOLDER-FIXTURE"
            nfName f `shouldBe` Just "Synthetic Folder"
          _ -> expectationFailure $ "expected 1 folder, got " <> show (length folders)

  describe "fetchRecent" $ do
    it "returns NoteSummary list from a query response" $
      withNotesMock queryNotesJson "/records/query" $ \ep api -> do
        notes <- fetchRecent api ep
        case notes of
          [n] -> do
            nsId n `shouldBe` NoteId "Note/NOTE-FIXTURE"
            nsTitle n `shouldBe` Just "Synthetic note"
          _ -> expectationFailure $ "expected 1 note, got " <> show (length notes)
    it "accumulates results across paginated responses" $
      withPaginatedMock $ \ep api -> do
        notes <- fetchRecent api ep
        length notes `shouldBe` 2

  describe "fetchNote" $ do
    it "returns Just Note with decoded body for a live record" $
      withNotesMock lookupNoteJson "/records/lookup" $ \ep api -> do
        result <- fetchNote api ep (NoteId "Note/NOTE-FIXTURE")
        case result of
          Nothing -> expectationFailure "expected Just Note"
          Just n -> noteBodyBytes n `shouldBe` "synthetic note body"
    it "returns Nothing for a tombstone record" $
      withNotesMock tombstoneLookupJson "/records/lookup" $ \ep api -> do
        result <- fetchNote api ep (NoteId "Note/NOTE-DELETED-FIXTURE")
        result `shouldBe` Nothing

  describe "fetchNotesInFolder" $ do
    it "returns only notes in the given folder, excluding other folders and deleted notes" $
      withNotesMock folderChangesJson "/changes/zone" $ \ep api -> do
        notes <- fetchNotesInFolder api ep (FolderId "Folder/FOLDER-FIXTURE")
        case notes of
          [n] -> nsId n `shouldBe` NoteId "Note/NOTE-FIXTURE"
          _ -> expectationFailure $ "expected 1 note, got " <> show (length notes)


-- Mock servers

withNotesMock
  :: LBS.ByteString
  -> BS.ByteString
  -> (NotesEndpoints -> Api -> IO a)
  -> IO a
withNotesMock json pathSuffix action =
  withSystemTempDirectory "icloud-notes-mock" $ \tmpDir ->
    testWithApplication (pure (simpleApp json pathSuffix)) $ \serverPort -> do
      (ep, api) <- mkEpAndApi serverPort tmpDir
      action ep api


withPaginatedMock :: (NotesEndpoints -> Api -> IO a) -> IO a
withPaginatedMock action =
  withSystemTempDirectory "icloud-notes-paginated" $ \tmpDir -> do
    callRef <- newIORef (0 :: Int)
    testWithApplication (pure (paginatedApp callRef)) $ \serverPort -> do
      (ep, api) <- mkEpAndApi serverPort tmpDir
      action ep api


simpleApp :: LBS.ByteString -> BS.ByteString -> Application
simpleApp json pathSuffix req respond
  | pathSuffix `BS.isSuffixOf` rawPathInfo req =
      respond $ responseLBS status200 jsonHeaders json
  | otherwise =
      respond $ responseLBS status404 [] "not found"


paginatedApp :: IORef Int -> Application
paginatedApp callRef _req respond = do
  n <- readIORef callRef
  writeIORef callRef (n + 1)
  respond $ responseLBS status200 jsonHeaders (if n == 0 then page1Json else page2Json)


mkEpAndApi :: Int -> FilePath -> IO (NotesEndpoints, Api)
mkEpAndApi serverPort tmpDir = do
  let baseUrl = Text.pack $ "http://127.0.0.1:" ++ show serverPort
  ep <- mkNotesEndpoints (testAccountData baseUrl) (testSession tmpDir)
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
    , adWebservices = Map.fromList [("ckdatabasews", baseUrl)]
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
    }
 where
  dummyReq p =
    defaultRequest
      { host = "127.0.0.1"
      , port = serverPort
      , secure = False
      , method = methodPost
      , path = p
      }


jsonHeaders :: [(HeaderName, BS8.ByteString)]
jsonHeaders = [(hContentType, "application/json")]


queryFoldersJson :: LBS.ByteString
queryFoldersJson =
  "{\"records\":[{\"recordName\":\"Folder/FOLDER-FIXTURE\"\
  \,\"recordType\":\"Folder\"\
  \,\"fields\":{\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIEZvbGRlcg==\"}}}]\
  \,\"continuationMarker\":null}"


queryNotesJson :: LBS.ByteString
queryNotesJson =
  "{\"records\":[{\"recordName\":\"Note/NOTE-FIXTURE\"\
  \,\"recordType\":\"Note\"\
  \,\"fields\":{\
  \\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIG5vdGU=\"}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}}}]\
  \,\"continuationMarker\":null}"


lookupNoteJson :: LBS.ByteString
lookupNoteJson =
  "{\"records\":[{\"recordName\":\"Note/NOTE-FIXTURE\"\
  \,\"recordType\":\"Note\"\
  \,\"fields\":{\
  \\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIG5vdGU=\"}\
  \,\"TextDataEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"c3ludGhldGljIG5vdGUgYm9keQ==\"}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}}}]}"


tombstoneLookupJson :: LBS.ByteString
tombstoneLookupJson =
  "{\"records\":[{\"recordName\":\"Note/NOTE-DELETED-FIXTURE\",\"deleted\":true}]}"


folderChangesJson :: LBS.ByteString
folderChangesJson =
  "{\"zones\":[{\"zoneID\":{\"zoneName\":\"Notes\",\"zoneType\":\"REGULAR_CUSTOM_ZONE\"}\
  \,\"syncToken\":\"sync-1\",\"moreComing\":false\
  \,\"records\":[\
  \{\"recordName\":\"Note/NOTE-FIXTURE\",\"recordType\":\"Note\"\
  \,\"fields\":{\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"U3ludGhldGljIG5vdGU=\"}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}\
  \,\"Folder\":{\"type\":\"REFERENCE\",\"value\":{\"recordName\":\"Folder/FOLDER-FIXTURE\",\"action\":\"VALIDATE\"}}}}\
  \,{\"recordName\":\"Note/NOTE-OTHER\",\"recordType\":\"Note\"\
  \,\"fields\":{\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"T3RoZXIgTm90ZQ==\"}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}\
  \,\"Folder\":{\"type\":\"REFERENCE\",\"value\":{\"recordName\":\"Folder/OTHER-FOLDER\",\"action\":\"VALIDATE\"}}}}\
  \,{\"recordName\":\"Note/NOTE-DELETED\",\"recordType\":\"Note\"\
  \,\"fields\":{\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"RGVsZXRlZCBOb3Rl\"}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":1}\
  \,\"Folder\":{\"type\":\"REFERENCE\",\"value\":{\"recordName\":\"Folder/FOLDER-FIXTURE\",\"action\":\"VALIDATE\"}}}}]}]}"


page1Json :: LBS.ByteString
page1Json =
  "{\"records\":[{\"recordName\":\"Note/NOTE-1\"\
  \,\"recordType\":\"Note\"\
  \,\"fields\":{\
  \\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"Tm90ZSAx\"}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}}}]\
  \,\"continuationMarker\":\"page-marker-1\"}"


page2Json :: LBS.ByteString
page2Json =
  "{\"records\":[{\"recordName\":\"Note/NOTE-2\"\
  \,\"recordType\":\"Note\"\
  \,\"fields\":{\
  \\"TitleEncrypted\":{\"type\":\"ENCRYPTED_BYTES\",\"value\":\"Tm90ZSAy\"}\
  \,\"Deleted\":{\"type\":\"INT64\",\"value\":0}}}]\
  \,\"continuationMarker\":null}"
