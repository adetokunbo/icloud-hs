{-# LANGUAGE OverloadedStrings #-}

module HStratus.Drive.EndpointsSpec (spec) where

import Data.Aeson (Value, decode, object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Network.HStratus.Internal.Drive.Endpoints
  ( downloadTokenReq
  , mkDriveEndpoints
  , nodeDetailsBody
  , nodeDetailsReq
  )
import Network.HStratus.Internal.Drive.Node (DriveNodeId (..), rootNodeId)
import Network.HStratus.Session (AccountData (..), Credentials (..), Session (..), Webservice (..))
import Network.HTTP.Client (Request (..))
import Network.HTTP.Types (methodGet, methodPost)
import Test.Hspec


spec :: Spec
spec = describe "Network.HStratus.Internal.Drive.Endpoints" $ do
  ep <- runIO (mkDriveEndpoints testAccountData testSession)

  describe "nodeDetailsBody" $ do
    it "encodes the node id and partialData=false" $
      nodeDetailsBody rootNodeId
        `shouldBe` ( "[{\"drivewsid\":\"FOLDER::com.apple.CloudDocs::root\""
                       <> ",\"partialData\":false}]"
                       :: LBS.ByteString
                   )
    it "produces valid JSON for node ids containing quotes or backslashes" $
      (decode (nodeDetailsBody (DriveNodeId "folder/with\"quote\\here")) :: Maybe Value)
        `shouldNotBe` Nothing

  describe "nodeDetailsReq" $ do
    it "targets retrieveItemDetailsInFolders" $
      path (nodeDetailsReq ep)
        `shouldSatisfy` BS.isSuffixOf "/retrieveItemDetailsInFolders"
    it "uses POST" $
      method (nodeDetailsReq ep) `shouldBe` methodPost
    it "includes clientId query param" $
      queryString (nodeDetailsReq ep)
        `shouldSatisfy` BS.isInfixOf "clientId=auth-test-client-id"

  describe "downloadTokenReq" $ do
    let req = downloadTokenReq "DOC-001" "com.apple.CloudDocs" ep
    it "targets /ws/<zone>/download/by_id" $
      path req
        `shouldSatisfy` BS.isSuffixOf "/ws/com.apple.CloudDocs/download/by_id"
    it "uses GET" $
      method req `shouldBe` methodGet
    it "includes clientId in query string" $
      queryString req `shouldSatisfy` BS.isInfixOf "clientId=auth-test-client-id"
    it "includes document_id in query string" $
      queryString req `shouldSatisfy` BS.isInfixOf "document_id=DOC-001"


-- Fixtures

testClientId :: Text
testClientId = "auth-test-client-id"


testAccountData :: AccountData
testAccountData =
  AccountData
    { adHsaVersion = 2
    , adHsaChallengeRequired = False
    , adHsaTrustedBrowser = Just True
    , adWebservices =
        Map.fromList
          [ ("drivews", Webservice "https://p31-drivews.icloud.com" Nothing)
          , ("docws", Webservice "https://p31-docws.icloud.com" Nothing)
          ]
    , adRaw = object []
    }


testSession :: Session
testSession =
  Session
    { sessionCreds = Credentials{credAccountName = "test@example.com", credPassword = "test-pass"}
    , sessionTopDir = "/tmp/test"
    , sessionClientId = testClientId
    }
