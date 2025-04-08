{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Crypto.SRP.PrimeGroup
Copyright : (c) 2025 Tim Emiola
Maintainer: Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Crypto.SRP.PrimeGroup
  ( -- * the PrimeGroups
    PrimeGroup (..)
  , generatorFor
  , safePrimeFor
  , asByteString
  , hexLength
  , paddedHexOfGenerator
  , pubOf
  , padAs
  , pow
  , primeMod
  , modExpPrime
  )
where

import Crypto.SRP.Constants
  ( fromHexBS
  , n1024Bits
  , n1536Bits
  , n2048Bits
  , n3072Bits
  , n4096Bits
  , n6144Bits
  , n8192Bits
  )
import Data.Bits (Bits (shiftR, testBit))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Fmt (build, fmt, hexF)


-- | Represents the primeGroups used in SRP computations
data PrimeGroup
  = G1024
  | G1536
  | G2048
  | G3072
  | G4096
  | G6144
  | G8192
  deriving (Eq, Show)


-- | The generator for 'PrimeGroup'
generatorFor :: PrimeGroup -> Word8
generatorFor G1024 = 0x2
generatorFor G1536 = 0x2
generatorFor G2048 = 0x2
generatorFor G3072 = 0x5
generatorFor G4096 = 0x5
generatorFor G6144 = 0x5
generatorFor G8192 = 0x19


-- | The safe prime for 'PrimeGroup'
safePrimeFor :: PrimeGroup -> Integer
safePrimeFor = fromHexBS . asByteString


-- | A ByteString representing the safe prime in hexadecimal
asByteString :: PrimeGroup -> ByteString
asByteString G1024 = n1024Bits
asByteString G1536 = n1536Bits
asByteString G2048 = n2048Bits
asByteString G3072 = n3072Bits
asByteString G4096 = n4096Bits
asByteString G6144 = n6144Bits
asByteString G8192 = n8192Bits


-- | The length of the result of 'asByteString'
hexLength :: PrimeGroup -> Int
hexLength = BS.length . asByteString


{- | A ByteString of the generator padded so that has the same length as the
result of 'asByteString'
-}
paddedHexOfGenerator :: PrimeGroup -> ByteString
paddedHexOfGenerator pg =
  let unpadded = fmt $ build $ hexF $ generatorFor pg
   in unpadded `padAs` pg


primeMod :: Integer -> PrimeGroup -> Integer
primeMod num pg =
  let prime = safePrimeFor pg
   in num `mod` prime


pow :: PrimeGroup -> Integer -> Integer
pow pg expn =
  let g = generatorFor pg
   in toInteger g ^ expn


{- | Pad a 'ByteString' so it's the same length as the serialized byte form of
the PrimeGroup
-}
padAs :: ByteString -> PrimeGroup -> ByteString
padAs bs pg =
  let
    padLength = hexLength pg - BS.length bs
   in
    BS.replicate padLength 0 <> bs


{- | Generate the public version of a private ephemeral key

the private version of the key is expected to be randomly generated value of
64 bits
-}
pubOf :: Integer -> PrimeGroup -> Integer
pubOf priv pg = modExpPrime (fromIntegral (generatorFor pg)) priv pg


{- | Perform exponetiation modulus the large number in a 'PrimeGroup'

Example

  > modExpPrime base power G2048
-}
modExpPrime :: Integer -> Integer -> PrimeGroup -> Integer
modExpPrime base power pg = modExp base power (safePrimeFor pg)


modExp :: Integer -> Integer -> Integer -> Integer
modExp _base 0 _m = 1
modExp base expn m = t * modExp baseSquared (shiftR expn 1) m `mod` m
 where
  !baseSquared = (base * base) `mod` m
  !t = if testBit expn 0 then base `mod` m else 1
