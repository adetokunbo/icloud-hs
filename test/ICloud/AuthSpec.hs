{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.AuthSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.AuthSpec (spec) where

import Data.ByteString (ByteString)
import Data.String.Conv (toS)
import Data.Text (Text)
import Network.HTTP.Types.Header (HeaderName)
import Network.ICloud.Auth (
  Credentials (..),
  Session (..),
  SessionData (..),
  cookiePath,
  hCounter,
  hCountry,
  hSessionId,
  hSessionToken,
  hTrustToken,
  mkSessionData,
  sessionPath,
 )
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.QuickCheck (
  Gen,
  Property,
  elements,
  forAllBlind,
  sublistOf,
  vectorOf,
 )


spec :: Spec
spec = do
  sessionSpec
  sessionDataSpec


sessionDataSpec :: Spec
sessionDataSpec = describe "SessionData" $ do
  context "using generated headers" $ do
    it "should generated the expected value" prop_mkSessionData


sessionSpec :: Spec
sessionSpec = describe "Session" $ do
  context "using a simple example" $ do
    context "cookiePath" $ do
      it "should be computed correctly" $ do
        let want = "/tmp/icloud_authspec/myaccountid-applecom.cookies.txt"
        cookiePath exampleSession `shouldBe` want

    context "sessionPath" $ do
      it "should be computed correctly" $ do
        let want = "/tmp/icloud_authspec/myaccountid-applecom.session.json"
        sessionPath exampleSession `shouldBe` want


exampleCred :: Credentials
exampleCred =
  Credentials
    { credAccountName = "my-account-id@apple.com"
    , credPassword = "notasecret"
    }


exampleSession :: Session
exampleSession =
  Session
    { sessionCreds = exampleCred
    , sessionTopDir = "/tmp/icloud_authspec"
    }


prop_mkSessionData :: Property
prop_mkSessionData = forAllBlind genHdrsAndExpectedSessionData $ \(hdrs, f) ->
  f $ mkSessionData hdrs


genHdrsAndExpectedSessionData :: Gen ([(HeaderName, ByteString)], SessionData -> Bool)
genHdrsAndExpectedSessionData = do
  checks <- sublistOf sdChecks
  values <- vectorOf (length checks) (elements exampleValues)
  let headers = zip (map fst checks) values
      asPred getter want sd = Just (toS want) == getter sd
      preds = zipWith asPred (map snd checks) values
      combine xs sd = all ($ sd) xs
  pure (headers, combine preds)


sdChecks :: [(HeaderName, SessionData -> Maybe Text)]
sdChecks =
  [ (hCountry, sdAccountCountry)
  , (hSessionId, sdSessionId)
  , (hSessionToken, sdSessionToken)
  , (hTrustToken, sdTrustToken)
  , (hCounter, sdCounter)
  ]


exampleValues :: [ByteString]
exampleValues =
  [ "Good"
  , "King"
  , "Wenceslas"
  , "looked"
  , "out"
  , "on"
  , "feast"
  , "of"
  , "stephen"
  , "when"
  , "snow"
  , "lay"
  , "round"
  , "about"
  , "bright"
  , "crisp"
  , "even"
  ]
