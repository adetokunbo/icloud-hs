{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.SessionSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.SessionSpec (spec) where

import Control.Monad (when)
import Data.Aeson (decode, eitherDecodeFileStrict, encode, encodeFile)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Word (Word16)
import ICloud.TrustSpec (jsonKeysOf)
import Network.ICloud.Internal.Session
  ( SavedHeaders (..)
  , accountDataRequires2FA
  , accountDataRequires2SA
  , appBase
  , clientIdPath
  , cookiePath
  , credentialsPath
  , loadAccountData
  , loadSavedHeaders
  , saveAccountData
  , saveCredentialsTo
  , savedHeadersPath
  , updateSessionSavedHeaders
  , (</>)
  )
import Network.ICloud.Session (AccountData (..), Credentials (..), Session (..), loadSession)
import System.Directory (createDirectory, doesFileExist)
import System.Environment (setEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
  ( Spec
  , anyIOException
  , around
  , context
  , describe
  , it
  , shouldBe
  , shouldReturn
  , shouldThrow
  )
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , Gen
  , Property
  , elements
  , frequency
  , listOf
  )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)


spec :: Spec
spec = do
  sessionSpec
  saveCredentialsSpec
  accountDataSpec


-- save credential somewhere
-- confirm Session loads

-- save credentials
-- save a pre-existing clientId
-- loadSession; confirm loaded session as the clientId

-- save credentials
-- save generated SavedHeaders
-- loadSession; confirm saveHeaders are loaded

sessionSpec :: Spec
sessionSpec = describe "module Network.ICloud.Session" $ do
  context "Using an example Credential" $ do
    let topDir = "/tmp/icloud_authspec"
    context "cookiePath" $ do
      it "should be computed correctly" $ do
        let want = "/tmp/icloud_authspec/myaccountid-applecom.cookies.txt"
        cookiePath topDir exampleCred `shouldBe` want

    context "savedHeadersPath" $ do
      it "should be computed correctly" $ do
        let want = "/tmp/icloud_authspec/myaccountid-applecom.session.json"
        savedHeadersPath topDir exampleCred `shouldBe` want

    context "clientIdPath" $ do
      it "should be computed correctly" $ do
        let want = "/tmp/icloud_authspec/myaccountid-applecom.client-id.txt"
        clientIdPath topDir exampleCred `shouldBe` want
  savedHeadersFieldNamesSpec
  loadSessionSpec
  loadSavedHeadersSpec
  updateSessionSavedHeadersSpec


savedHeadersFieldNamesSpec :: Spec
savedHeadersFieldNamesSpec = describe "SavedHeaders JSON field names" $ do
  it "uses the expected field names" $
    jsonKeysOf (SavedHeaders (Just "a") (Just "b") (Just "c") (Just "d") (Just "e"))
      `shouldBe` Just (sort ["country", "session_id", "session_token", "trust_token", "counter"])


loadSessionSpec :: Spec
loadSessionSpec = describe "loadSession" $ around useTmp $ do
  context "with an invalid credentials file" $ do
    it "should fail to load" $ \appRoot ->
      failsOnBadCredentials appRoot `shouldThrow` anyIOException
  context "with only the credentials file" $ do
    it "should load a Session with a new clientId" prop_loadsSession
  context "with a clientId file available" $ do
    it "should load the saved clientId" prop_readsStoredCliendId
  context "with a saved headers file" $ do
    it "should load the saved headers" prop_readsStoredSavedHeaders


loadSavedHeadersSpec :: Spec
loadSavedHeadersSpec = describe "loadSavedHeaders" $ around useTmp $ do
  context "with an invalid saved headers file" $ do
    it "should fail to load" $ \appRoot ->
      failsOnBadSavedHeaders appRoot `shouldThrow` anyIOException


updateSessionSavedHeadersSpec :: Spec
updateSessionSavedHeadersSpec = describe "updateSessionSavedHeaders" $ around useTmp $ do
  context "when some SaveHeaders are already saved" $ do
    it "should update to the new headers" $ prop_updatesSavedHeaders True
  context "when No SaveHeaders have been saved" $ do
    it "should update to the new headers" $ prop_updatesSavedHeaders True


useTmp :: (FilePath -> IO a) -> IO a
useTmp = withSystemTempDirectory "icloud-auth" . asConfigHome


setupInvalid :: FilePath -> IO ()
setupInvalid path = Text.writeFile path "[}"


failsOnBadCredentials :: FilePath -> IO Session
failsOnBadCredentials appRoot = do
  setupInvalid (credentialsPath appRoot)
  loadSession


failsOnBadSavedHeaders :: FilePath -> IO SavedHeaders
failsOnBadSavedHeaders appRoot = do
  encodeFile (credentialsPath appRoot) exampleCred
  setupInvalid (savedHeadersPath appRoot exampleCred)
  s <- loadSession
  loadSavedHeaders s


asConfigHome :: (FilePath -> IO a) -> FilePath -> IO a
asConfigHome action root = do
  setEnv "XDG_CONFIG_HOME" root
  let appRoot = root </> appBase
  createDirectory appRoot
  action appRoot


prop_loadsSession :: FilePath -> Property
prop_loadsSession appRoot = monadicIO $ do
  preCreds <- pick genPreCredentials
  let creds = asCreds preCreds
  s <- run $ do
    encodeFile (credentialsPath appRoot) creds
    loadSession
  assert $ sessionClientId s /= "" && creds == sessionCreds s


prop_readsStoredCliendId :: FilePath -> Property
prop_readsStoredCliendId appRoot = monadicIO $ do
  preCreds <- pick genPreCredentials
  fakeId <- pick $ genIndexedSuffix "client-id-"
  let creds = asCreds preCreds
  session <- run $ do
    encodeFile (credentialsPath appRoot) creds
    Text.writeFile (clientIdPath appRoot creds) fakeId
    loadSession
  assert $ fakeId == sessionClientId session


prop_readsStoredSavedHeaders :: FilePath -> Property
prop_readsStoredSavedHeaders appRoot = monadicIO $ do
  preCreds <- pick genPreCredentials
  savedHdrs <- pick genSaveHeaders
  let creds = asCreds preCreds
  savedHdrs' <- run $ do
    encodeFile (credentialsPath appRoot) creds
    encodeFile (savedHeadersPath appRoot creds) savedHdrs
    s <- loadSession
    loadSavedHeaders s
  assert $ savedHdrs == savedHdrs'


prop_updatesSavedHeaders :: Bool -> FilePath -> Property
prop_updatesSavedHeaders storeInitial appRoot = monadicIO $ do
  preCreds <- pick genPreCredentials
  savedHdrs <- pick genSaveHeaders
  newHdrs <- pick genSaveHeaders
  let creds = asCreds preCreds
  loadedHdrs <- run $ do
    encodeFile (credentialsPath appRoot) creds
    when storeInitial $
      encodeFile (savedHeadersPath appRoot creds) savedHdrs
    s <- loadSession
    updateSessionSavedHeaders s (const newHdrs)
    loadSavedHeaders s
  assert $ newHdrs == loadedHdrs


saveCredentialsSpec :: Spec
saveCredentialsSpec = describe "saveCredentialsTo" $ do
  context "in a pre-existing directory" $ around withTmpDir $ do
    it "creates the credentials file" $ \tmpDir -> do
      saveCredentialsTo tmpDir exampleCred
      doesFileExist (credentialsPath tmpDir) `shouldReturn` True
    it "round-trips credentials through JSON" prop_saveLoadCredentials
    it "overwrites when called a second time" prop_overwritesCredentials
  context "when the target directory does not exist" $ do
    it "creates the directory and the file" $
      withSystemTempDirectory "icloud-auth-creds" $ \tmp -> do
        let target = tmp </> "new-subdir"
        saveCredentialsTo target exampleCred
        doesFileExist (credentialsPath target) `shouldReturn` True


withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = withSystemTempDirectory "icloud-auth-creds"


prop_saveLoadCredentials :: FilePath -> Property
prop_saveLoadCredentials tmpDir = monadicIO $ do
  creds <- asCreds <$> pick genPreCredentials
  result <- run $ do
    saveCredentialsTo tmpDir creds
    eitherDecodeFileStrict (credentialsPath tmpDir)
  assert $ result == Right creds


prop_overwritesCredentials :: FilePath -> Property
prop_overwritesCredentials tmpDir = monadicIO $ do
  creds1 <- asCreds <$> pick genPreCredentials
  creds2 <- asCreds <$> pick genPreCredentials
  result <- run $ do
    saveCredentialsTo tmpDir creds1
    saveCredentialsTo tmpDir creds2
    eitherDecodeFileStrict (credentialsPath tmpDir)
  assert $ result == Right creds2


exampleCred :: Credentials
exampleCred =
  Credentials
    { credAccountName = "my-account-id@apple.com"
    , credPassword = "notasecret"
    }


type PreCredentials = (Text, Text)


asCreds :: PreCredentials -> Credentials
asCreds (credAccountName, credPassword) = Credentials{credAccountName, credPassword}


genPreCredentials :: Gen PreCredentials
genPreCredentials =
  let mkId x = "account-" <> x <> "@apple.com"
   in (,) <$> genIndexedTemplate mkId <*> genIndexedSuffix "password-"


genSaveHeaders :: Gen SavedHeaders
genSaveHeaders =
  let arb pre = frequency [(2, pure Nothing), (1, Just <$> genIndexedSuffix pre)]
   in SavedHeaders
        <$> arb "country-"
        <*> arb "session-id-"
        <*> arb "session-token-"
        <*> arb "trust-token-"
        <*> arb "counter="


genWord16 :: Gen Word16
genWord16 = arbitrary


genIndexedSuffix :: (Monoid a, IsString a) => a -> Gen a
genIndexedSuffix pre = genIndexedTemplate (pre <>)


genIndexedTemplate :: (IsString a) => (a -> a) -> Gen a
genIndexedTemplate plate = plate . fromString . show <$> genWord16


accountDataSpec :: Spec
accountDataSpec = describe "module Network.ICloud.Session (AccountData)" $ do
  context "AccountData" $ do
    it "round-trips through JSON encoding" prop_jsonRoundtripAccountData
  context "accountDataRequires2FA" $ do
    it "is True when hsaVersion == 2 and challenged" $
      accountDataRequires2FA (mkAccountData 2 True False) `shouldBe` True
    it "is True when hsaVersion == 2, not challenged, but browser untrusted" $
      accountDataRequires2FA (mkAccountData 2 False False) `shouldBe` True
    it "is False when hsaVersion == 2, not challenged, and browser trusted" $
      accountDataRequires2FA (mkAccountData 2 False True) `shouldBe` False
    it "is False when hsaVersion is 1" $
      accountDataRequires2FA (mkAccountData 1 True False) `shouldBe` False
    it "is False when hsaVersion is 3 (unknown version)" $
      accountDataRequires2FA (mkAccountData 3 True False) `shouldBe` False
  context "accountDataRequires2SA" $ do
    it "is True when hsaVersion is 1" $
      accountDataRequires2SA (mkAccountData 1 False False) `shouldBe` True
    it "is False when hsaVersion is 2" $
      accountDataRequires2SA (mkAccountData 2 False False) `shouldBe` False
    it "is False when hsaVersion is 0" $
      accountDataRequires2SA (mkAccountData 0 False False) `shouldBe` False
  context "AccountData JSON parsing" $ do
    it "fails to parse from null JSON" $
      (decode "null" :: Maybe AccountData) `shouldBe` Nothing
    it "fails to parse when dsInfo is absent" $
      (decode "{}" :: Maybe AccountData) `shouldBe` Nothing
  context "saveAccountData / loadAccountData" $ around useTmp $ do
    it "round-trips in a temp directory" prop_saveLoadAccountData


prop_jsonRoundtripAccountData :: Property
prop_jsonRoundtripAccountData = monadicIO $ do
  ad <- pick genAccountData
  assert $ decode (encode ad) == Just ad


prop_saveLoadAccountData :: FilePath -> Property
prop_saveLoadAccountData appRoot = monadicIO $ do
  preCreds <- pick genPreCredentials
  ad <- pick genAccountData
  let creds = asCreds preCreds
  loaded <- run $ do
    encodeFile (credentialsPath appRoot) creds
    s <- loadSession
    saveAccountData s ad
    loadAccountData s
  assert $ Just ad == loaded


mkAccountData :: Int -> Bool -> Bool -> AccountData
mkAccountData ver challenged trusted =
  AccountData
    { adHsaVersion = ver
    , adHsaChallengeRequired = challenged
    , adHsaTrustedBrowser = trusted
    , adWebservices = Map.empty
    }


genAccountData :: Gen AccountData
genAccountData = do
  adHsaVersion <- abs <$> arbitrary
  adHsaChallengeRequired <- arbitrary
  adHsaTrustedBrowser <- arbitrary
  adWebservices <- Map.fromList <$> listOf genWsPair
  pure AccountData{adHsaVersion, adHsaChallengeRequired, adHsaTrustedBrowser, adWebservices}
 where
  genWsPair = (,) <$> elements wsNames <*> genIndexedSuffix "https://example.com/"
  wsNames = ["findme", "contacts", "calendar", "mail"]
