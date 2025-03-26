{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Crypto.SRP
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Crypto.SRP (
  -- * client-side inputs
  FromClient (..),
  mkFromClient,

  -- * server-side inputs
  FromServer (..),

  -- * shared key and proofs
  Results (..),
  calcResults,
  verifyServerProof,

  -- * Integer <=> ByteString
  bytesOf,
  fromBytes,

  -- * re-exports
  PrimeGroup (..),
  KnownAlgorithm (..),
) where

import Crypto.SRP.Hashing (
  KnownAlgorithm (..),
  calcClientX,
  calcCombinedPubKeys,
  calcK,
  calcXorHashnHashg,
  hash,
  hashMany,
  hashText,
 )
import Crypto.SRP.PrimeGroup (
  PrimeGroup (..),
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


data Results = Results
  { rKey :: !ByteString
  , rClientProof :: !ByteString
  , rServerProof :: !ByteString
  }
  deriving (Eq)


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
  serverProof == rServerProof (calcResults fc fs)


{- | Calculate the shared session key and proofs

  K = H(S) -- S is the premaster secret, K is the shared session key

  M (clientProof) is calculated independently on the server and client and is
  sent from the cient to the server. If this does not match the server's value
  the server aborts the authentication process.  The client calculates this as:

  M = H(H(N) XOR H(g) | H(U) | s | A | B | K)

  AMK (serverProof) is also calculated on both the server and client, but it's
  sent by the server to the client after the server accepts the clientProof
  received from the client

  AMK = H(A | M | K)

  if the serverProof does not match what the client expects, it aborts
-}
calcResults :: FromClient -> FromServer -> Results
calcResults fc fs =
  let FromServer {fsPublicBytes, fsSalt, fsPrimeGroup = pg, fsKnownAlgorithm = alg} = fs
      FromClient {fcUser, fcPublicBytes = publicBytes} = fc
      bigS = calcPremasterSecret fc fs
      xorNG = bytesOf $ calcXorHashnHashg alg pg
      hashedName = hashText alg fcUser
      rKey = hash alg $ bytesOf bigS
      rClientProof = hashMany alg [xorNG, hashedName, fsSalt, publicBytes, fsPublicBytes, rKey]
      rServerProof = hashMany alg [publicBytes, rClientProof, rKey]
   in Results {rKey, rClientProof, rServerProof}


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


-- | Obtain an @Integer@ from its @ByteString@ encoding
fromBytes :: ByteString -> Integer
fromBytes = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0


-- | Encode an @Integer@ as a @ByteString@
bytesOf :: Integer -> BS.ByteString
bytesOf n
  | n == 0 = BS.pack [0]
  | otherwise = BS.pack $ reverse (bytes (abs n))
  where
    bytes 0 = []
    bytes x = fromIntegral (x .&. 0xFF) : bytes (shiftR x 8)
