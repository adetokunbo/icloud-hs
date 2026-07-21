{-# LANGUAGE OverloadedStrings #-}

module ICloud.ApiLoggerSpec (spec) where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (isPrefixOf)
import ICloud.Mock (defaultScenario, withMockApp)
import Network.HTTP.Client (Request (..), defaultManagerSettings, defaultRequest, newManager)
import Network.HTTP.Types (methodPost)
import Network.ICloud.Http (ApiLogger, fileLogger, login, mkApiWith, redactingLogger, withLogger)
import Network.ICloud.Http.Endpoints (Endpoints (..))
import Network.ICloud.Session (Credentials (..), Session (..))
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), withFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldContain, shouldNotBe, shouldNotContain, shouldSatisfy)


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

  describe "redactingLogger" $ do
    it "replaces sensitive header values with <redacted>" $
      withSystemTempDirectory "icloud-auth-redact" $ \tmpDir -> do
        let logPath = tmpDir </> "requests.log"
        withLoginLogUsing redactingLogger tmpDir logPath $ \contents ->
          contents `shouldContain` "<redacted>"
    it "preserves the method, URI and status line" $
      withSystemTempDirectory "icloud-auth-redact" $ \tmpDir -> do
        let logPath = tmpDir </> "requests.log"
        withLoginLogUsing redactingLogger tmpDir logPath $ \contents -> do
          contents `shouldContain` "POST"
          contents `shouldContain` "signin/init"
          contents `shouldContain` "200"
    it "does not write raw Set-Cookie values" $
      withSystemTempDirectory "icloud-auth-redact" $ \tmpDir -> do
        let logPath = tmpDir </> "requests.log"
        withLoginLogUsing fileLogger tmpDir logPath $ \verboseContents ->
          withLoginLogUsing redactingLogger tmpDir (logPath <> ".redacted") $ \redactedContents -> do
            let cookieLines = filter ("Set-Cookie:" `isPrefixOf`) (lines verboseContents)
            case cookieLines of
              [] -> pure ()
              (firstLine : _) -> redactedContents `shouldNotContain` drop (length ("Set-Cookie: " :: String)) firstLine


{- | Extract the body of the first log entry.
Format: summary line, request headers, blank line, response headers, blank line, body, "---".
-}
firstBody :: String -> String
firstBody contents =
  let skipSection = drop 1 . dropWhile (not . null)
      bodyLines = takeWhile (/= "---") $ skipSection $ skipSection $ drop 1 (lines contents)
   in unlines bodyLines


withLoginLog :: FilePath -> FilePath -> (String -> IO a) -> IO a
withLoginLog = withLoginLogUsing fileLogger


withLoginLogUsing :: (Handle -> ApiLogger) -> FilePath -> FilePath -> (String -> IO a) -> IO a
withLoginLogUsing mkLogger tmpDir logPath action = do
  let sessionDir = tmpDir </> "session"
  withFile logPath WriteMode $ \logHandle ->
    withMockApp defaultScenario $ \serverPort -> do
      mgr <- newManager defaultManagerSettings
      api <-
        withLogger (mkLogger logHandle)
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
