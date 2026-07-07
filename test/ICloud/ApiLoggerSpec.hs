{-# LANGUAGE OverloadedStrings #-}

module ICloud.ApiLoggerSpec (spec) where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import ICloud.Mock (defaultScenario, withMockApp)
import Network.HTTP.Client (Request (..), defaultManagerSettings, defaultRequest, newManager)
import Network.HTTP.Types (methodPost)
import Network.ICloud.Http (fileLogger, login, mkApiWith, withLogger)
import Network.ICloud.Http.Endpoints (Endpoints (..))
import Network.ICloud.Session (Credentials (..), Session (..))
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldContain, shouldNotBe, shouldSatisfy)


spec :: Spec
spec = describe "Network.ICloud.Http.fileLogger" $ do
  it "writes a log entry for each HTTP request during login" $
    withSystemTempDirectory "icloud-auth-log" $ \tmpDir -> do
      let logPath = tmpDir </> "requests.log"
      withLoginLog tmpDir logPath $ \contents ->
        filter (== "---") (lines contents) `shouldSatisfy` (not . null)

  it "each entry contains the HTTP method, URI and response status" $
    withSystemTempDirectory "icloud-auth-log" $ \tmpDir -> do
      let logPath = tmpDir </> "requests.log"
      withLoginLog tmpDir logPath $ \contents -> do
        contents `shouldContain` "POST"
        contents `shouldContain` "signin/init"
        contents `shouldContain` "200"

  it "response bodies are valid JSON" $
    withSystemTempDirectory "icloud-auth-log" $ \tmpDir -> do
      let logPath = tmpDir </> "requests.log"
      withLoginLog tmpDir logPath $ \contents -> do
        let body = firstBody contents
        (decode (LBS8.pack body) :: Maybe Value) `shouldNotBe` Nothing


-- | Extract the body of the first log entry (all lines between the header and the first separator).
firstBody :: String -> String
firstBody contents =
  let bodyLines = takeWhile (/= "---") (drop 1 (lines contents))
   in unlines bodyLines


withLoginLog :: FilePath -> FilePath -> (String -> IO a) -> IO a
withLoginLog tmpDir logPath action = do
  let sessionDir = tmpDir </> "session"
  withFile logPath WriteMode $ \logHandle ->
    withMockApp defaultScenario $ \serverPort -> do
      mgr <- newManager defaultManagerSettings
      api <-
        withLogger (fileLogger logHandle)
          <$> mkApiWith (testSession sessionDir) (testEndpoints serverPort) mgr
      _ <- login api
      pure ()
  contents <- readFile logPath
  action contents


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
