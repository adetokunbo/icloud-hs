{-# LANGUAGE NamedFieldPuns #-}

-- Converts gzip-compressed protobuf note bodies to the domain NoteText type.
-- This is the bridge between Internal.Notes.Proto (wire representation) and
-- the public NoteText/NoteRun/NoteStyle types.
--
-- Note: GZip.decompress is lazy.  If the input bytes are not valid gzip, a
-- runtime exception is thrown when the decompressed stream is first consumed.
-- Callers that need to handle corrupt payloads gracefully should wrap
-- decodeNoteBody in Control.Exception.try.
module Network.ICloud.Internal.Notes.Decode
  ( decodeNoteBody
  )
where

import qualified Codec.Compression.GZip as GZip
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.ICloud.Internal.Notes.Note
  ( NoteRun (..)
  , NoteStyle (..)
  , NoteText (..)
  )
import Network.ICloud.Internal.Notes.Proto
  ( ProtoAttributeRun (..)
  , ProtoNote (..)
  , ProtoParagraphStyle (..)
  , decodeNoteStoreProto
  )


{- | Decode a note body.  The input is the raw bytes from the CloudKit
@TextDataEncrypted@ field after base64-decoding (Phase 1 does this in
'noteRecordToNote').  The encoding is: gzip( protobuf( NoteStoreProto ) ).
-}
decodeNoteBody :: ByteString -> Either String NoteText
decodeNoteBody =
  fmap toNoteText
    . decodeNoteStoreProto
    . LBS.toStrict
    . GZip.decompress
    . LBS.fromStrict


-- Map ProtoNote → NoteText.  The proto layer mirrors the wire schema; this
-- function applies the domain interpretation (e.g. font_weight int → bold/italic
-- booleans, empty link string → Nothing).
toNoteText :: ProtoNote -> NoteText
toNoteText ProtoNote{pnNoteText, pnAttributeRuns} =
  NoteText
    { ntText = pnNoteText
    , ntRuns = map toNoteRun pnAttributeRuns
    }


toNoteRun :: ProtoAttributeRun -> NoteRun
toNoteRun ProtoAttributeRun{parLength, parParagraphStyle, parFontWeight, parUnderlined, parLink} =
  NoteRun
    { nrLength = parLength
    , nrStyle = parParagraphStyle >>= toNoteStyle
    , -- FontWeight enum: 1=bold, 2=italic, 3=bold+italic
      nrBold = parFontWeight == 1 || parFontWeight == 3
    , nrItalic = parFontWeight == 2 || parFontWeight == 3
    , nrUnderline = parUnderlined /= 0
    , -- proto3-wire yields lazy Text; empty string means no link
      nrLink = let t = LT.toStrict parLink in if T.null t then Nothing else Just t
    }


-- StyleType enum from notes.proto.  Values not in this list represent
-- future or unknown styles and are mapped to Nothing so the domain layer
-- can render them as unstyled rather than failing.
toNoteStyle :: ProtoParagraphStyle -> Maybe NoteStyle
toNoteStyle (ProtoParagraphStyle n) = case n of
  0 -> Just StyleTitle
  1 -> Just StyleHeading
  2 -> Just StyleSubheading
  4 -> Just StyleMonospaced
  100 -> Just StyleBullet
  101 -> Just StyleDash
  102 -> Just StyleNumbered
  103 -> Just StyleChecklist
  _ -> Nothing
