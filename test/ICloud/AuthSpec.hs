{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.AuthSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.AuthSpec (spec) where

import Test.Hspec
import Network.ICloud.Auth

spec :: Spec
spec = describe "Auth" $ do
  context "endsThen" $
    it "should be a simple test" $ do
      getIt `endsThen` (== (Just "a string"))


getIt :: IO (Maybe String)
getIt = pure $ Just "a string"
