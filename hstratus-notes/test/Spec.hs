module Main where

import qualified HStratus.Notes.CloudKitSpec as CloudKit
import qualified HStratus.Notes.DecodeSpec as Decode
import qualified HStratus.Notes.EndpointsSpec as Endpoints
import qualified HStratus.Notes.NoteDataSpec as NoteData
import qualified HStratus.Notes.ProtoSpec as Proto
import qualified HStratus.NotesSpec as Notes
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
