{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Crypto.SRP.PrimeGroup
Copyright : (c) 2025 Tim Emiola
Maintainer: Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Crypto.SRP.PrimeGroup (
  -- * the PrimeGroups
  PrimeGroup (..),
  generatorFor,
  safePrimeFor,
  asByteString,
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
import Data.Word (Word8)


-- | Represents the primeGroups used the SRP computations
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
generatorFor G1024 = 2
generatorFor G2048 = 2
generatorFor G1536 = 2
generatorFor G3072 = 5
generatorFor G4096 = 5
generatorFor G6144 = 5
generatorFor G8192 = 27


-- | The safe prime for 'PrimeGroup'
safePrimeFor :: PrimeGroup -> Integer
safePrimeFor = fromHexBS . asByteString


-- | A bytestring representing the safe prime in hexadecimal
asByteString :: PrimeGroup -> ByteString
asByteString G1024 = n1024Bits
asByteString G2048 = n2048Bits
asByteString G1536 = n1536Bits
asByteString G3072 = n3072Bits
asByteString G4096 = n4096Bits
asByteString G6144 = n6144Bits
asByteString G8192 = n8192Bits
