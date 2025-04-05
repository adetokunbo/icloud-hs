{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Crypto.SRP.Hash
Copyright : (c) 2025 Tim Emiola
Maintainer: Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Crypto.SRP.Hashing (
  KnownAlgorithm (..),
  Algorithm (..),
  alg,
  hash,
  hashMany,
  hashText,
  calcK,
  calcClientX,
  calcXorHashnHashg,
) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA384 as SHA384
import qualified Crypto.Hash.SHA512 as SHA512
import Crypto.SRP.PrimeGroup (
  PrimeGroup,
  asByteString,
  padAs,
  paddedHexOfGenerator,
 )
import Data.Bits (Bits (xor))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Normalize (NormalizationMode (NFKC), normalize)
import Data.Word (Word8)


fromBytes :: ByteString -> Integer
fromBytes = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0




-- | Compute the multiplier 'k' as described in the SRP RFC
calcK :: KnownAlgorithm -> PrimeGroup -> ByteString
calcK known pg =
  hashMany
    known
    [ asByteString pg
    , paddedHexOfGenerator pg
    ]


-- | Compute the multiplier 'k' as described in the SRP RFC
calcXorHashnHashg :: KnownAlgorithm -> PrimeGroup -> Integer
calcXorHashnHashg known pg =
  let hashedN = fromBytes $ hash known (asByteString pg)
      hashedG = fromBytes $ hash known (paddedHexOfGenerator pg)
   in hashedN `xor` hashedG




-- | Compute the hash 'x' with the client session calculation
calcClientX :: (Text, Text) -> ByteString -> KnownAlgorithm -> ByteString
calcClientX (username, password) serverSalt known =
  let h = hashMany known
      normalize' = encodeUtf8 . normalize NFKC
   in h [serverSalt, h [normalize' username, ":", normalize' password]]


hashText :: KnownAlgorithm -> Text -> ByteString
hashText known txt =
  let
    normalize' = encodeUtf8 . normalize NFKC
   in
    hash known $ normalize' txt


-- | Provides an interface to the implemention an hash algorithm
data Algorithm = Algorithm
  { algDigestSize :: {-# UNPACK #-} !Word8
  , algHash :: !(ByteString -> ByteString)
  , algHashMany :: !([ByteString] -> ByteString)
  }


-- | Implement the hash function of a 'KnownAlgorithm'
hash :: KnownAlgorithm -> ByteString -> ByteString
hash = algHash . alg


-- | Implement the hash function of a 'KnownAlgorithm'
hashMany :: KnownAlgorithm -> [ByteString] -> ByteString
hashMany = algHashMany . alg


-- | Enumerates the specific hash algorithms that this SRP implementation supports
data KnownAlgorithm
  = SHA1
  | SHA256
  | SHA384
  | SHA512
  deriving (Eq, Show)


-- | Provides an 'Algorithm' that contains the implementation for each 'KnownAlgorithm'
alg :: KnownAlgorithm -> Algorithm
alg SHA1 = Algorithm 20 SHA1.hash (SHA1.finalize . SHA1.updates SHA1.init)
alg SHA256 = Algorithm 32 SHA256.hash (SHA256.finalize . SHA256.updates SHA256.init)
alg SHA384 = Algorithm 48 SHA384.hash (SHA384.finalize . SHA384.updates SHA384.init)
alg SHA512 = Algorithm 64 SHA512.hash (SHA512.finalize . SHA512.updates SHA512.init)
