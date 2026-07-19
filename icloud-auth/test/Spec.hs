{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified ICloud.ApiLoggerSpec as ApiLogger
import qualified ICloud.Http.EndpointsSpec as HttpEndpoints
import qualified ICloud.Http.ErrorsSpec as HttpErrors
import qualified ICloud.Http.HeadersSpec as HttpHeaders
import qualified ICloud.HttpMockSpec as HttpMock
import qualified ICloud.HttpSpec as Http
import qualified ICloud.LoginFSMSpec as LoginFSM
import qualified ICloud.PBKDF2Spec as PBKDF2
import qualified ICloud.SessionSpec as Session
import qualified ICloud.TrustSpec as Trust
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
    ApiLogger.spec
    HttpEndpoints.spec
    HttpErrors.spec
    HttpHeaders.spec
    HttpMock.spec
    PBKDF2.spec
    Trust.spec
    LoginFSM.spec
