{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified ICloud.Drive.EndpointsSpec as DriveEndpoints
import qualified ICloud.Drive.MutationSpec as DriveMutation
import qualified ICloud.Drive.NodeSpec as DriveNode
import qualified ICloud.Drive.UploadSpec as DriveUpload
import qualified ICloud.DriveSpec as Drive
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
    DriveNode.spec
    DriveEndpoints.spec
    Drive.spec
    DriveMutation.spec
    DriveUpload.spec
