module Main where

import qualified Hstratus.Cli.AuthSpec as Auth
import System.IO
  ( BufferMode (..)
  , hSetBuffering
  , stderr
  , stdout
  )
import Test.Hspec


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hspec Auth.spec
