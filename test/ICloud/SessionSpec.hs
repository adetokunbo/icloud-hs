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
import Data.Aeson (encodeFile)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Word (Word16)
import Network.ICloud.Session
  ( Credentials (..)
  , SavedHeaders (..)
  , Session (..)
  , appBase
  , clientIdPath
  , cookiePath
  , credentialsPath
  , loadSession
  , savedHeadersPath
  , updateSessionSavedHeaders
  , (</>)
  )
import System.Directory (createDirectory)
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
  , shouldThrow
  )
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , Gen
  , Property
  , frequency
  )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)


spec :: Spec
spec = do
  sessionSpec


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
  loadSessionSpec
  updateSessionSavedHeadersSpec


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


failsOnBadSavedHeaders :: FilePath -> IO Session
failsOnBadSavedHeaders appRoot = do
  encodeFile (credentialsPath appRoot) exampleCred
  setupInvalid (savedHeadersPath appRoot exampleCred)
  loadSession


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
  session <- run $ do
    encodeFile (credentialsPath appRoot) creds
    encodeFile (savedHeadersPath appRoot creds) savedHdrs
    loadSession
  assert $ savedHdrs == sessionSavedHdrs session


prop_updatesSavedHeaders :: Bool -> FilePath -> Property
prop_updatesSavedHeaders storeInitial appRoot = monadicIO $ do
  preCreds <- pick genPreCredentials
  savedHdrs <- pick genSaveHeaders
  newHdrs <- pick genSaveHeaders
  let creds = asCreds preCreds
  session <- run $ do
    encodeFile (credentialsPath appRoot) creds
    when storeInitial $
      encodeFile (savedHeadersPath appRoot creds) savedHdrs
    s <- loadSession
    updateSessionSavedHeaders s (const newHdrs)
  assert $ newHdrs == sessionSavedHdrs session


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
