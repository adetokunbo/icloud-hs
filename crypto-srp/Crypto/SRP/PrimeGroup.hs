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
) where

import Crypto.SRP.Constants (
  n1024Bits,
  n1536Bits,
  n2048Bits,
  n3072Bits,
  n4096Bits,
  n6144Bits,
  n8192Bits,
  sumBytes,
 )
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


-- | The generator for a 'PrimeGroup'
generatorFor :: PrimeGroup -> Word8
generatorFor G1024 = 2
generatorFor G2048 = 2
generatorFor G1536 = 2
generatorFor G3072 = 5
generatorFor G4096 = 5
generatorFor G6144 = 5
generatorFor G8192 = 19


-- | The large safe prime for 'PrimeGroup'
safePrimeFor :: PrimeGroup -> Integer
safePrimeFor G1024 = sumBytes n1024Bits
safePrimeFor G2048 = sumBytes n2048Bits
safePrimeFor G1536 = sumBytes n1536Bits
safePrimeFor G3072 = sumBytes n3072Bits
safePrimeFor G4096 = sumBytes n4096Bits
safePrimeFor G6144 = sumBytes n6144Bits
safePrimeFor G8192 = sumBytes n8192Bits
