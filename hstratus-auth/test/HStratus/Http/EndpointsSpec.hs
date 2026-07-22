{-# LANGUAGE OverloadedStrings #-}

module HStratus.Http.EndpointsSpec (spec) where

import qualified Data.Map.Strict as Map
import Network.HStratus.Internal.Endpoints (Realm (..), lookupWebservice, realmEndpoints, signinCompleteBase)
import Network.HStratus.Internal.HttpErrors (AuthError (..))
import Network.HStratus.Internal.Session (Webservice (..))
import Network.HTTP.Client (Request (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)


spec :: Spec
spec = describe "Network.HStratus.Internal.Endpoints" $ do
  it "signinCompleteBase queryString has no leading ?" $
    queryString (signinCompleteBase (realmEndpoints Usual)) `shouldBe` "isRememberMeEnabled=true"

  describe "lookupWebservice" $ do
    it "resolves an active service to a Request" $ do
      let ws = Map.fromList [("findme", Webservice "https://p01-fmipweb.icloud.com" (Just "active"))]
      req <- lookupWebservice "findme" ws
      host req `shouldBe` "p01-fmipweb.icloud.com"

    it "resolves a service with no status to a Request" $ do
      let ws = Map.fromList [("findme", Webservice "https://p01-fmipweb.icloud.com" Nothing)]
      req <- lookupWebservice "findme" ws
      host req `shouldBe` "p01-fmipweb.icloud.com"

    it "throws WebserviceNotFound for an absent key" $
      lookupWebservice "missing" Map.empty
        `shouldThrow` (== WebserviceNotFound "missing")

    it "throws WebserviceNotFound for an inactive service" $ do
      let ws = Map.fromList [("findme", Webservice "https://p01-fmipweb.icloud.com" (Just "inactive"))]
      lookupWebservice "findme" ws
        `shouldThrow` (== WebserviceNotFound "findme")
