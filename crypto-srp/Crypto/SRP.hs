{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Crypto.SRP
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Crypto.SRP (
  FromClient (..),
  FromServer (..),
  mkFromClient,
  calcKeyAndProof,
  verifyServerProof,
  fromBytes,
  bytesOf,
) where

import Crypto.SRP.Hashing (
  KnownAlgorithm,
  calcClientX,
  calcCombinedPubKeys,
  calcK,
  calcXorHashnHashg,
  hash,
  hashMany,
  hashText,
 )
import Crypto.SRP.PrimeGroup (
  PrimeGroup,
  modExpPrime,
  primeMod,
  pubOf,
 )
import Crypto.SRP.Random (genNSecureBytes)
import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)


-- | Identifies a user
type Username = Text


-- | A user's cleartext password
type Password = Text


data FromServer = FromServer
  { fsPublicBytes :: !ByteString
  , fsSalt :: !ByteString
  , fsPrimeGroup :: !PrimeGroup
  , fsKnownAlgorithm :: !KnownAlgorithm
  }
  deriving (Eq)


data FromClient = FromClient
  { fcUser :: !Username
  , fcPassword :: !Password
  , fcPrivateNumber :: !Integer
  , fcPublicBytes :: !ByteString
  }


{- | Build a 'FromClient', generating the public and private epheremal values
required for the client-side of the authentication process
-}
mkFromClient :: Username -> Password -> PrimeGroup -> IO FromClient
mkFromClient fcUser fcPassword pg = do
  privateBytes <- genNSecureBytes 32
  let private = fromBytes privateBytes
      public = private `pubOf` pg
  pure
    FromClient
      { fcUser
      , fcPassword
      , fcPublicBytes = bytesOf public
      , fcPrivateNumber = fromBytes privateBytes
      }


verifyServerProof :: ByteString -> FromClient -> FromServer -> Bool
verifyServerProof serverProof fc fs =
  let (theKey, theProof) = calcKeyAndProof fc fs
      clientProof = hashMany (fsKnownAlgorithm fs) [fcPublicBytes fc, theProof, theKey]
   in clientProof == serverProof


{- | Calculate the session key and proof

  K = H(S) -- S is the premaster secret
  M = H(H(N) XOR H(g) | H(U) | s | A | B | K)
-}
calcKeyAndProof :: FromClient -> FromServer -> (ByteString, ByteString)
calcKeyAndProof fc fs =
  let FromServer {fsPublicBytes, fsSalt, fsPrimeGroup = pg, fsKnownAlgorithm = alg} = fs
      FromClient {fcUser, fcPublicBytes = publicBytes} = fc
      bigS = calcPremasterSecret fc fs
      xorNG = bytesOf $ calcXorHashnHashg alg pg
      hashedName = hashText alg fcUser
      theKey = hash alg $ bytesOf bigS
      theProof = hashMany alg [xorNG, hashedName, fsSalt, publicBytes, fsPublicBytes, theKey]
   in (theKey, theProof)


{- |
The premaster secret is calculated by the client as follows:
    I, P = <read from user>
    N, g, s, B = <read from server>
    a = random()
    A = g^a % N
    u = H(PAD(A) | PAD(B))
    k = H(N | PAD(g))
    x = H(s | HA1(I | ":" | P))
    <premaster secret> = (B - (k * g^x)) ^ (a + (u * x)) % N
      == ((B - (k * g^x)) % N) ^ (a + (u * x)) % N
      == (((B % N) - ((k * g^x) % N)) % N) ^ (a + (u *x)) % N
-}
calcPremasterSecret :: FromClient -> FromServer -> Integer
calcPremasterSecret fc fs =
  let
    FromServer {fsPublicBytes, fsSalt, fsPrimeGroup = pg, fsKnownAlgorithm = alg} = fs
    FromClient {fcUser, fcPassword, fcPrivateNumber = private, fcPublicBytes = publicBytes} = fc
    x = fromBytes $ calcClientX (fcUser, fcPassword) fsSalt alg
    u = fromBytes $ calcCombinedPubKeys publicBytes fsPublicBytes alg pg
    power = private + (u * x)
    x' = x `pubOf` pg
    bigB = fromBytes fsPublicBytes
    k = fromBytes $ calcK alg pg
    base = ((bigB `primeMod` pg) - ((k * x') `primeMod` pg)) `primeMod` pg
   in
    modExpPrime base power pg


fromBytes :: ByteString -> Integer
fromBytes = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0


bytesOf :: Integer -> BS.ByteString
bytesOf n
  | n == 0 = BS.pack [0]
  | otherwise = BS.pack $ reverse (bytes (abs n))
  where
    bytes 0 = []
    bytes x = fromIntegral (x .&. 0xFF) : bytes (shiftR x 8)
