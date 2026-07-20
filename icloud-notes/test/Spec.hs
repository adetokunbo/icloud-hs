module Main where

import qualified ICloud.Notes.CloudKitSpec as CloudKit
import qualified ICloud.Notes.DecodeSpec as Decode
import qualified ICloud.Notes.EndpointsSpec as Endpoints
import qualified ICloud.Notes.NoteDataSpec as NoteData
import qualified ICloud.Notes.ProtoSpec as Proto
import qualified ICloud.NotesSpec as Notes
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
    CloudKit.spec
    Decode.spec
    Endpoints.spec
    NoteData.spec
    Proto.spec
    Notes.spec
