{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified HStratus.ApiLoggerSpec as ApiLogger
import qualified HStratus.Http.CliSpec as HttpCli
import qualified HStratus.Http.EndpointsSpec as HttpEndpoints
import qualified HStratus.Http.ErrorsSpec as HttpErrors
import qualified HStratus.Http.HeadersSpec as HttpHeaders
import qualified HStratus.HttpMockSpec as HttpMock
import qualified HStratus.HttpSpec as Http
import qualified HStratus.LoginFSMSpec as LoginFSM
import qualified HStratus.PBKDF2Spec as PBKDF2
import qualified HStratus.SessionSpec as Session
import qualified HStratus.TrustSpec as Trust
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
    HttpCli.spec
    HttpEndpoints.spec
    HttpErrors.spec
    HttpHeaders.spec
    HttpMock.spec
    PBKDF2.spec
    Trust.spec
    LoginFSM.spec
