{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.DriveSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.DriveSpec (spec) where

import Test.Hspec
import Network.ICloud.Drive

spec :: Spec
spec = describe "module Network.ICloud.Drive" $ do
  context "endsThen" $
    it "should be a simple test" $ do
      getIt `endsThen` (== (Just "a string"))


getIt :: IO (Maybe String)
getIt = pure $ Just "a string"
