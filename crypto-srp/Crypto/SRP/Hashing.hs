{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Crypto.SRP.Hash
Copyright : (c) 2025 Tim Emiola
Maintainer: Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Crypto.SRP.Hashing
  ( -- * Supported hash algorithms
    KnownAlgorithm (..)

    -- ** Use the @'KnownAlgorithm's@ for hashing
  , digestSize
  , hash
  , hashMany
  , hashText

    -- * SRP-specific hash calculations
  , calcK
  , calcClientX
  , calcXorHashnHashg
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA384 as SHA384
import qualified Crypto.Hash.SHA512 as SHA512
import Crypto.SRP.PrimeGroup
  ( PrimeGroup
  , asByteString
  , paddedHexOfGenerator
  )
import Data.Bits (Bits (xor))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Normalize (NormalizationMode (NFKC), normalize)
import Data.Word (Word32)


fromBytes :: ByteString -> Integer
fromBytes = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0


{- | Compute the multiplier \'k\' as a step in the calculation of the premaster
   secret

See [premaster secret calculation](https://datatracker.ietf.org/doc/html/rfc5054#section-2.6)
-}
calcK :: KnownAlgorithm -> PrimeGroup -> ByteString
calcK known pg =
  hashMany
    known
    [ asByteString pg
    , paddedHexOfGenerator pg
    ]


-- | Compute an XORed hash describing a @'PrimeGroup'@.
calcXorHashnHashg :: KnownAlgorithm -> PrimeGroup -> Integer
calcXorHashnHashg known pg =
  let hashedN = fromBytes $ hash known (asByteString pg)
      hashedG = fromBytes $ hash known (paddedHexOfGenerator pg)
   in hashedN `xor` hashedG


{- | Compute the hash \'x\' as a step in the calculation of the premaster secret

See [premaster secret calculation](https://datatracker.ietf.org/doc/html/rfc5054#section-2.6)
-}
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


-- | Provides an interface to the implemention of a hash algorithm
data Algorithm = Algorithm
  { algDigestSize :: {-# UNPACK #-} !Word32
  , algHash :: !(ByteString -> ByteString)
  , algHashMany :: !([ByteString] -> ByteString)
  }


-- | Hash a @ByteString@ using the hash function of a 'KnownAlgorithm'
hash :: KnownAlgorithm -> ByteString -> ByteString
hash = algHash . alg


-- | Hash several @ByteStrings@ using the hash function of a 'KnownAlgorithm'
hashMany :: KnownAlgorithm -> [ByteString] -> ByteString
hashMany = algHashMany . alg


-- | The size of digest computed by a 'KnownAlgorithm'
digestSize :: KnownAlgorithm -> Word32
digestSize = algDigestSize . alg


-- | Enumerates specific hash algorithms supported by this package
data KnownAlgorithm
  = SHA1
  | SHA256
  | SHA384
  | SHA512
  deriving (Eq, Show)


-- Provides an 'Algorithm' that contains the implementation for each 'KnownAlgorithm'
alg :: KnownAlgorithm -> Algorithm
alg SHA1 = Algorithm 20 SHA1.hash (SHA1.finalize . SHA1.updates SHA1.init)
alg SHA256 = Algorithm 32 SHA256.hash (SHA256.finalize . SHA256.updates SHA256.init)
alg SHA384 = Algorithm 48 SHA384.hash (SHA384.finalize . SHA384.updates SHA384.init)
alg SHA512 = Algorithm 64 SHA512.hash (SHA512.finalize . SHA512.updates SHA512.init)
