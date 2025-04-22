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
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified ICloud.Examples as Examples
import Network.ICloud.Trust
import System.IO.Silently (silence)
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
import Test.Main (withStdin)
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , Gen
  , Property
  , chooseInt
  , elements
  , forAll
  , frequency
  , listOf1
  , vectorOf
  )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)


spec :: Spec
spec = describe "module Network.ICloud.Trust" $ do
  describe "TrustData" $ do
    context "parsing generated examples to/from JSON" $ do
      it "should succeed" prop_jsonRoundtripTrustData

  describe "selectPhone" $ do
    context "the selected input" $ do
      context "is a number within the range" $ do
        it "should succeed" prop_selectsPhoneWithNonMaxIndex
      context "is the maximum number" $ do
        it "should succeed" prop_selectsPhoneWithMaxIndex


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


prop_selectsPhoneWithNonMaxIndex :: Property
prop_selectsPhoneWithNonMaxIndex = monadicIO $ do
  pick genTrustedPhonesWithNonMaxIndex >>= run . tryToSelectPhone >>= assert


prop_selectsPhoneWithMaxIndex :: Property
prop_selectsPhoneWithMaxIndex = monadicIO $ do
  let useMax (phone, phones) = (length phones, phone, phones)
  pick (fmap useMax genTrustedPhonesWithMaxIndex) >>= run . tryToSelectPhone >>= assert


tryToSelectPhone :: (Int, TrustedPhone, [TrustedPhone]) -> IO Bool
tryToSelectPhone (idx, want, phones) = do
  withStdin (toS $ show idx) $ do
    selected <- silence $ selectPhone phones
    pure $ selected == want


genTrustedPhonesWithNonMaxIndex :: Gen (Int, TrustedPhone, [TrustedPhone])
genTrustedPhonesWithNonMaxIndex = do
  low <- chooseInt (1, 5)
  high <- chooseInt (low, 10)
  phones <- vectorOf high genTrustedPhone
  pure (low, phones !! (low - 1), phones)


genTrustedPhonesWithMaxIndex :: Gen (TrustedPhone, [TrustedPhone])
genTrustedPhonesWithMaxIndex = do
  low <- chooseInt (1, 5)
  high <- chooseInt (low, 10)
  phones <- vectorOf high genTrustedPhone
  pure (phones !! (high - 1), phones)


genTrustData :: Gen TrustData
genTrustData = TrustData <$> genTrustedList <*> genCodeStatus <*> arbitrary


genExWord :: Gen Text
genExWord = elements Examples.wordz


genExWordMaybe :: Gen (Maybe Text)
genExWordMaybe = frequency [(1, pure Nothing), (1, Just <$> genExWord)]
