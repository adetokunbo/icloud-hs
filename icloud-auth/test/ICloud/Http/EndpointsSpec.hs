{-# LANGUAGE OverloadedStrings #-}

module ICloud.Http.EndpointsSpec (spec) where

import Network.HTTP.Client (Request (..))
import Network.ICloud.Internal.Endpoints (Realm (..), realmEndpoints, signinCompleteBase)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = describe "Network.ICloud.Internal.Endpoints" $ do
  it "signinCompleteBase queryString has no leading ?" $
    queryString (signinCompleteBase (realmEndpoints Usual)) `shouldBe` "isRememberMeEnabled=true"
