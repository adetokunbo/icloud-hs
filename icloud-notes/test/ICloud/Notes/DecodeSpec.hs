{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ICloud.Notes.DecodeSpec (spec) where

import Data.ByteString (ByteString)
import Network.ICloud.Internal.Notes.Decode (decodeNoteBody)
import Network.ICloud.Internal.Notes.Note (NoteText (..))
import Test.Hspec


spec :: Spec
spec = describe "decodeNoteBody" $ do
  it "decodes a gzip-compressed protobuf note and extracts the text" $
    case decodeNoteBody fixtureBytes of
      Left err -> expectationFailure err
      Right NoteText{ntText, ntRuns} -> do
        ntText `shouldBe` "Step 6b test"
        ntRuns `shouldBe` []

  it "returns Left for bytes that are valid gzip but empty protobuf" $
    case decodeNoteBody emptyNoteBytes of
      Left _ -> pure ()
      Right _ -> expectationFailure "expected Left for missing note field"


-- gzip( NoteStoreProto { document=2: Document { note=3: Note { note_text=2: "Step 6b test" } } } )
-- Generated with mtime=0 for determinism.
-- Proto hex: 12101a0e120c537465702036622074657374
fixtureBytes :: ByteString
fixtureBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\xff\x13\x12\x90\xe2\x13\xe2\x09\x2e\
  \\x49\x2d\x50\x30\x4b\x52\x28\x49\x2d\x2e\x01\x00\x41\xcb\xcc\x34\x12\x00\
  \\x00\x00"


-- gzip( NoteStoreProto {} ) — valid gzip, but the document field is absent,
-- so decodeNoteBody should return Left "NoteStoreProto: document field absent".
emptyNoteBytes :: ByteString
emptyNoteBytes =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\xff\x03\x00\x00\x00\x00\x00\x00\x00\
  \\x00\x00"
