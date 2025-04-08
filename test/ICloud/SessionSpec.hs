{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.SessionSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.SessionSpec (spec) where

import Network.ICloud.Session
  ( Credentials (..)
  , clientIdPath
  , cookiePath
  , savedHeadersPath
  )
import Test.Hspec (Spec, context, describe, it, shouldBe)


spec :: Spec
spec = do
  sessionSpec


sessionSpec :: Spec
sessionSpec = describe "module Network.ICloud.Session" $ do
  context "using a simple example" $ do
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


exampleCred :: Credentials
exampleCred =
  Credentials
    { credAccountName = "my-account-id@apple.com"
    , credPassword = "notasecret"
    }
