{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Crypto.SRPSpec as SRP
import qualified ICloud.HttpSpec as Http
import qualified ICloud.PBKDF2Spec as PBKDF2
import qualified ICloud.SessionSpec as Session
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
    Session.spec
    Http.spec
    PBKDF2.spec
    SRP.spec
