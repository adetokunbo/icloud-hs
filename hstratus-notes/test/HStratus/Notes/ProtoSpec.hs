{-# LANGUAGE OverloadedStrings #-}

module HStratus.Notes.ProtoSpec (spec) where

import Data.ByteString (ByteString)
import Network.HStratus.Internal.Notes.Proto
import Test.Hspec


spec :: Spec
spec = describe "decodeNoteStoreProto" $ do
  it "decodes a minimal note with text only" $
    case decodeNoteStoreProto minimalNoteBytes of
      Left err -> expectationFailure err
      Right note -> do
        pnNoteText note `shouldBe` "hello"
        pnAttributeRuns note `shouldBe` []

  it "returns an error for empty input" $
    case decodeNoteStoreProto "" of
      Left _ -> pure ()
      Right _ -> expectationFailure "expected error for empty bytes"

  it "decodes attribute run length and paragraph style" $
    case decodeNoteStoreProto noteWithRunBytes of
      Left err -> expectationFailure err
      Right note -> do
        pnNoteText note `shouldBe` "hi"
        case pnAttributeRuns note of
          [run] -> do
            parLength run `shouldBe` 2
            parParagraphStyle run `shouldBe` Just (ProtoParagraphStyle 1)
          runs -> expectationFailure $ "expected 1 run, got " <> show (length runs)


-- NoteStoreProto { document: Document { note: Note { note_text: "hello" } } }
--
-- Note bytes    (field 2, wire 2): 0x12 0x05 "hello"              (7 bytes)
-- Document bytes(field 3, wire 2): 0x1a 0x07 <Note>               (9 bytes)
-- Proto bytes   (field 2, wire 2): 0x12 0x09 <Document>           (11 bytes)
minimalNoteBytes :: ByteString
minimalNoteBytes = "\x12\x09\x1a\x07\x12\x05hello"


-- NoteStoreProto { document: Document { note: Note {
--   note_text: "hi",
--   attribute_run: [ AttributeRun { length: 2, paragraph_style: ParagraphStyle { style_type: 1 } } ]
-- } } }
--
-- ParagraphStyle bytes (field 1, wire 0): 0x08 0x01              (2 bytes)
-- AttributeRun bytes:
--   field 1 (length=2, wire 0): 0x08 0x02
--   field 2 (paragraph_style, wire 2): 0x12 0x02 <ParagraphStyle>
--   total: 6 bytes
-- Note bytes (field 2: "hi", field 5: AttributeRun):
--   field 2 (text "hi", wire 2): 0x12 0x02 "hi"
--   field 5 (run, wire 2): 0x2a 0x06 <AttributeRun>
--   total: 12 bytes
-- Document bytes (field 3, wire 2): 0x1a 0x0c <Note>             (14 bytes)
-- Proto bytes   (field 2, wire 2): 0x12 0x0e <Document>          (16 bytes)
noteWithRunBytes :: ByteString
noteWithRunBytes =
  "\x12\x0e\x1a\x0c\x12\x02hi\x2a\x06\x08\x02\x12\x02\x08\x01"
