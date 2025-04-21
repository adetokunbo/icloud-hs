{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.TrustSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.TrustSpec (spec, encode, genTrustData, genTrustedList) where

import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified ICloud.Examples as Examples
import Network.ICloud.Trust
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
  , elements
  , forAll
  , frequency
  , listOf1
  )


spec :: Spec
spec = describe "module Network.ICloud.Trust" $ do
  describe "TrustData" $ do
    context "parsing generated examples to/from JSON" $ do
      it "should succeed" prop_jsonRoundtripTrustData


prop_jsonRoundtripTrustData :: Property
prop_jsonRoundtripTrustData = forAll genTrustData $ \td ->
  decode (encode td) == Just td


genCodeStatus :: Gen CodeStatus
genCodeStatus =
  CodeStatus
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


genTrustedPhone :: Gen TrustedPhone
genTrustedPhone = TrustedPhone <$> arbitrary <*> genExWord <*> genExWordMaybe


genTrustedDevice :: Gen TrustedDevice
genTrustedDevice = TrustedDevice <$> genExWord <*> genExWord <*> genExWord


genTrustedList :: Gen TrustedList
genTrustedList =
  frequency
    [ (1, TrustedPhoneNumbers <$> listOf1 genTrustedPhone)
    , (1, TrustedDevices <$> listOf1 genTrustedDevice)
    ]


genTrustData :: Gen TrustData
genTrustData = TrustData <$> genTrustedList <*> genCodeStatus <*> arbitrary


genExWord :: Gen Text
genExWord = elements Examples.wordz


genExWordMaybe :: Gen (Maybe Text)
genExWordMaybe = frequency [(1, pure Nothing), (1, Just <$> genExWord)]
