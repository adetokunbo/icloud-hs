{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.AuthSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.AuthSpec (spec) where

import Network.ICloud.Auth (
  Credentials (..),
  Session (..),
  cookiePath,
  sessionPath,
 )
import Test.Hspec (Spec, context, describe, it, shouldBe)


spec :: Spec
spec = describe "Session" $ do
  context "using a simple example" $ do
    context "sessionPath" $ do
      it "should be computed correctly" $ do
        let want = "/tmp/icloud_authspec/myaccountid-applecom.cookies.txt"
        cookiePath exampleSession `shouldBe` want

    context "cookiePath" $ do
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
