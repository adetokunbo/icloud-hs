{-# LANGUAGE OverloadedStrings #-}

module Crypto.SRPSpec (
  spec,
) where

import Crypto.SRP.Constants (
  fromHexBS,
  n1024Bits,
  n1536Bits,
  n2048Bits,
  n3072Bits,
  n4096Bits,
  n6144Bits,
  n8192Bits,
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Word (Word8)
import Fmt (build, fmt, hexF, (+|), (|+))
import Test.Hspec (Spec, context, describe, it)


spec :: Spec
spec = describe "module Crypto.SRP.Constants" $ do
  largeNumberSpec


largeNumberSpec :: Spec
largeNumberSpec = describe "the fixed large numbers" $ do
  oneNumberSpec n1024Bits 1024
  oneNumberSpec n1536Bits 1536
  oneNumberSpec n2048Bits 2048
  oneNumberSpec n3072Bits 3072
  oneNumberSpec n4096Bits 4096
  oneNumberSpec n6144Bits 6144
  oneNumberSpec n8192Bits 8192


oneNumberSpec :: ByteString -> Int -> Spec
oneNumberSpec b bitSize = do
  context ("the ByteString representing the " +| bitSize |+ " bit number") $ do
    context "each byte" $ do
      it "should be a valid hexadecimal value" $ isAllHex b
    it "should roundtrip with its integer value" $ fromHexBS b == fromHexBS (bsShow (fromHexBS b))


isHexChar :: Word8 -> Bool
isHexChar w = w - ordAlt '0' < 10 || w - ordAlt 'A' < 6 || w - ordAlt 'a' < 6


ordAlt :: Char -> Word8
ordAlt = fromIntegral . ord


isAllHex :: ByteString -> Bool
isAllHex b =
  let
    checkWord _ignored False = False
    checkWord nextChar True = isHexChar nextChar
   in
    BS.foldr checkWord True b


bsShow :: Integer -> ByteString
bsShow = fmt . build . hexF
