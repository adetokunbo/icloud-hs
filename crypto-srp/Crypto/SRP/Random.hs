{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Crypto.SRP.Random
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Crypto.SRP.Random (
  genNSecureBytes,
  gen256BitInteger,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Entropy (getEntropy, getHardwareEntropy)


-- | Get a specific number of bytes of cryptographically secure random data
genNSecureBytes :: Int -> IO ByteString
genNSecureBytes n = maybe (getEntropy n) pure =<< getHardwareEntropy n


gen256BitInteger :: IO Integer
gen256BitInteger = fromBytes <$> genNSecureBytes 32


fromBytes :: ByteString -> Integer
fromBytes = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0
