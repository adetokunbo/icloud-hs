{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified HStratus.Drive.EndpointsSpec as DriveEndpoints
import qualified HStratus.Drive.MutationSpec as DriveMutation
import qualified HStratus.Drive.NodeSpec as DriveNode
import qualified HStratus.Drive.UploadSpec as DriveUpload
import qualified HStratus.DriveSpec as Drive
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
