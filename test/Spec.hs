{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Crypto.SRPSpec as SRP
import qualified ICloud.AuthSpec as Auth
import qualified ICloud.HttpSpec as Http
import qualified ICloud.KDFSpec as KDF
import System.IO
  ( BufferMode (..)
  , hSetBuffering
  , stderr
  , stdout
  )
import Test.Hspec


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hspec $ do
    Auth.spec
    Http.spec
    KDF.spec
    SRP.spec
