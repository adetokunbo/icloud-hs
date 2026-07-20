-- Hand-written proto3 wire decoders for the Notes protobuf schema.
--
-- Normally these would be generated from notes.proto by a tool (protoc +
-- plugin, or proto3-suite's code-gen mode).  We use proto3-wire's lower-level
-- Parser API directly because the schema has non-consecutive field numbers
-- throughout (e.g. Note.note_text = 2, attribute_run = 5), and proto3-suite's
-- Generic Message derivation assigns field numbers by declaration order, which
-- would silently misdecode those gaps.  With explicit `at N` calls we bind
-- each record field to its actual proto field number.
--
-- The relevant message chain (from notes.proto):
--
--   NoteStoreProto
--     document = 2 : Document
--       note = 3 : Note
--         note_text = 2 : string
--         attribute_run = 5 : repeated AttributeRun
--           length = 1 : int32
--           paragraph_style = 2 : ParagraphStyle
--             style_type = 1 : int32   (StyleType enum)
--           font_weight = 5 : int32    (FontWeight enum: 1=bold, 2=italic, 3=both)
--           underlined = 6 : int32
--           strikethrough = 7 : int32
--           link = 9 : string
--
-- Fields not listed here are silently ignored by the wire decoder.
module Network.ICloud.Internal.Notes.Proto
  ( ProtoNote (..)
  , ProtoAttributeRun (..)
  , ProtoParagraphStyle (..)
  , decodeNoteStoreProto
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Proto3.Wire.Decode
  ( Parser
  , RawMessage
  , at
  , embedded
  , embedded'
  , int32
  , one
  , parse
  , repeated
  , text
  )


-- Proto types mirror the schema closely; conversion to domain NoteText/NoteRun
-- types happens in Decode.hs where gzip decompression also lives.

data ProtoNote = ProtoNote
  { pnNoteText :: Text
  , pnAttributeRuns :: [ProtoAttributeRun]
  }
  deriving (Eq, Show)


data ProtoAttributeRun = ProtoAttributeRun
  { parLength :: Int32
  , parParagraphStyle :: Maybe ProtoParagraphStyle
  , -- font_weight: 0=none, 1=bold, 2=italic, 3=bold+italic
    parFontWeight :: Int32
  , parUnderlined :: Int32
  , parStrikethrough :: Int32
  , -- empty string means no link
    parLink :: LT.Text
  }
  deriving (Eq, Show)


-- style_type values: 0=title, 1=heading, 2=subheading, 4=monospaced,
-- 100=bullet, 101=dash, 102=numbered, 103=checklist
newtype ProtoParagraphStyle = ProtoParagraphStyle
  { ppsStyleType :: Int32
  }
  deriving (Eq, Show)


{- | Decode a gzip-decompressed protobuf ByteString into a 'ProtoNote'.
Returns 'Left' with a message if the outer document or note field is absent,
which would indicate a malformed or empty payload rather than a real note.
-}
decodeNoteStoreProto :: ByteString -> Either String ProtoNote
decodeNoteStoreProto bs =
  case parse parseNoteStoreProto bs of
    Left err -> Left (show err)
    Right Nothing -> Left "NoteStoreProto: document field absent"
    Right (Just Nothing) -> Left "Document: note field absent"
    Right (Just (Just note)) -> Right note


-- Drill straight through NoteStoreProto (field 2) → Document (field 3) → Note
-- without defining a separate Document record type.  Each `embedded` call wraps
-- the result in Maybe: Nothing means the field was absent in the wire bytes.
parseNoteStoreProto :: Parser RawMessage (Maybe (Maybe ProtoNote))
parseNoteStoreProto = embedded (embedded parseProtoNote `at` 3) `at` 2


-- `one text LT.empty` reads a singular string field, returning the default
-- (empty) when the field is absent.  `fmap LT.toStrict` converts the lazy
-- Text that proto3-wire produces to the strict Text used in ProtoNote.
-- `repeated (embedded' ...)` collects all occurrences of a length-delimited
-- field into a list; embedded' (vs embedded) is used inside repeated because
-- the field is known to be present (not optional) at each occurrence.
parseProtoNote :: Parser RawMessage ProtoNote
parseProtoNote =
  ProtoNote
    <$> (fmap LT.toStrict (one text LT.empty) `at` 2)
    <*> (repeated (embedded' parseProtoAttributeRun) `at` 5)


-- Scalar optional fields (font_weight, underlined, strikethrough) use
-- `one int32 0` — the proto3 default of 0 means "absent / no effect" for
-- all of them (0 = FONT_WEIGHT_UNKNOWN, 0 = not underlined, etc.).
-- `embedded` for paragraph_style returns Maybe: Nothing when the run carries
-- no paragraph-level formatting.
parseProtoAttributeRun :: Parser RawMessage ProtoAttributeRun
parseProtoAttributeRun =
  ProtoAttributeRun
    <$> (one int32 0 `at` 1)
    <*> (embedded parseParagraphStyle `at` 2)
    <*> (one int32 0 `at` 5) -- font_weight (field 3 and 4 are absent in schema)
    <*> (one int32 0 `at` 6) -- underlined
    <*> (one int32 0 `at` 7) -- strikethrough
    <*> (one text LT.empty `at` 9) -- link (fields 8 skipped: superscript)


-- style_type is at field 1; remaining ParagraphStyle fields (alignment,
-- indent, checklist, etc.) are not needed by the domain layer and are ignored.
parseParagraphStyle :: Parser RawMessage ProtoParagraphStyle
parseParagraphStyle = ProtoParagraphStyle <$> (one int32 0 `at` 1)
