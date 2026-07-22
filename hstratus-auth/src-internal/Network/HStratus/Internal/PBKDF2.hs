{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Module      : Network.HStratus.PBKDF2
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Copied then modified from an implementation in the package
[ppad-pbkdf](https://git.ppad.tech/pbkdf/file/lib/Crypto/KDF/PBKDF.hs.html)

Re-implemented here rather than making it direct dependency, because:
    - 1 fewer dependency => less future dependency-related maintenance
    - faster route for this package to stackage
        - as of (2025/04/01, ppad-ppbkdf was not on stackage)
-}
module Network.HStratus.Internal.PBKDF2
  ( -- * specify a pseudorandom function and derived key length
    FancyPseudoRandomF
  , wrap
  , wrapIO
  , PseudoRandomF
  , BadKeyLength (..)

    -- * perform PBKDF2 derivation
  , deriveKey

    -- * re-export
  , ByteString
  )
where

import Control.Exception (Exception, throwIO)
import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Builder.Extra
  ( safeStrategy
  , smallChunkSize
  , toLazyByteStringWith
  )
import Data.Word (Word32, Word64)


{- | A pseudorandom function for use in PBKDF2

See
[PBKDF-RFC/section5.2](https://datatracker.ietf.org/doc/html/rfc2898#section-5.2)
-}
type PseudoRandomF = ByteString -> ByteString -> ByteString


-- | Indicates the derived key length is too long
data BadKeyLength = TooLong
  deriving (Eq, Show)


instance Exception BadKeyLength


{- | A 'PseudoRandomF' wrapped up with @dkLen@ and @hLen@'

where @dkLen@ the required length in octets of the derived key
and @hLen@ is the length of the output of the 'PseudoRandomF'

As per
[PBKDF-RFC/section5.2](https://datatracker.ietf.org/doc/html/rfc2898#section-5.2)

@dkLen@ must be at most 2^32 - 1 * @hLen@

The constructor `wrap` enforces this constraint
-}
newtype FancyPseudoRandomF = Fancy (PseudoRandomF, Word32, Word32)


-- | Construct a 'FancyPseudoRandomF'
wrap :: PseudoRandomF -> Word32 -> Either BadKeyLength FancyPseudoRandomF
wrap f dkLen =
  let !hLen = toNum $ BS.length $ f mempty mempty
   in if dkLen > 0xffffffff * hLen
        then Left TooLong
        else Right $ Fancy (f, dkLen, hLen)


-- | Like 'wrap', but fails by throwing 'BadKeyLength' in IO
wrapIO :: PseudoRandomF -> Word32 -> IO FancyPseudoRandomF
wrapIO f = either throwIO pure . wrap f


blockInfoOf :: FancyPseudoRandomF -> (Word32, Int)
blockInfoOf (Fancy (_f, !dkLen, hLen)) =
  let numBlocks = ceiling (toNum dkLen / toNum hLen :: Double)
      lastBlockSize = toNum $ dkLen - (numBlocks - 1) * hLen
   in (numBlocks, lastBlockSize)


{- | Derive a key from a secret using PBKDF2

Implements the key derivation algorithm described in
[PBKDF-RFC](https://datatracker.ietf.org/doc/html/rfc2898)

Usage - this example uses the SHA256 hmac function as the pseudorandom function

  >>> :set -XOverloadedStrings
  >>> import qualified Crypto.Hash.SHA256 as SHA256
  >>> pseudoF <- wrapIO SHA256.hmac 64
  >>> deriveKey pseudoF "passwd" "salt" 1000
-}
deriveKey
  :: FancyPseudoRandomF
  -- ^ a 'FancyPseudoRandomF'
  -> ByteString
  -- ^ the password from which to derive a key
  -> ByteString
  -- ^ the salt used in key derivation
  -> Word64
  -- ^ the iteration count
  -> ByteString
deriveKey fancy password salt count =
  let Fancy (!pseudoRandomF, !dkLen, !_notUsed) = fancy
      (!numBlocks, !lastBlockSize) = blockInfoOf fancy
      xorSum i =
        let initial = pseudoRandomF password $ salt <> asBytes i
            go j !current !_ignored | j == count = current
            go j !current !previous =
              let latest = pseudoRandomF password previous
               in go (j + 1) (current `xorBytes` latest) latest
         in go 1 initial initial
      {-# INLINE xorSum #-}

      smaller = safeStrategy 128 smallChunkSize
      strictBS =
        if dkLen <= 128
          then BS.toStrict . toLazyByteStringWith smaller mempty
          else BS.toStrict . toLazyByteString
      {-# INLINE strictBS #-}

      genBlocks i acc =
        if i < numBlocks
          then genBlocks (i + 1) (acc <> byteString (xorSum i))
          else strictBS $ acc <> byteString (BS.take lastBlockSize $ xorSum i)
   in genBlocks 1 mempty


toNum :: (Integral a, Num b) => a -> b
toNum = fromIntegral
{-# INLINE toNum #-}


asBytes :: Word32 -> ByteString
asBytes x =
  let !mask = 0b00000000000000000000000011111111
      !word0 = toNum (x `shiftR` 24) .&. mask
      !word1 = toNum (x `shiftR` 16) .&. mask
      !word2 = toNum (x `shiftR` 08) .&. mask
      !word3 = toNum x .&. mask
   in BS.cons word0 $ BS.cons word1 $ BS.cons word2 $ BS.singleton word3
{-# INLINE asBytes #-}


xorBytes :: ByteString -> ByteString -> ByteString
xorBytes = BS.packZipWith xor
{-# INLINE xorBytes #-}
