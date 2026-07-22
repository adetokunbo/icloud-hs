{-# LANGUAGE OverloadedStrings #-}

module HStratus.Examples
  ( byteStrings
  , wordz
  , errorKeys
  , codeKeys
  )
where

import Data.ByteString (ByteString)
import Data.String.Conv (toS)
import Data.Text (Text)


byteStrings :: [ByteString]
byteStrings =
  [ "Good"
  , "King"
  , "Wenceslas"
  , "looked"
  , "out"
  , "on"
  , "feast"
  , "of"
  , "stephen"
  , "when"
  , "snow"
  , "lay"
  , "round"
  , "about"
  , "bright"
  , "crisp"
  , "even"
  ]


wordz :: [Text]
wordz = map toS byteStrings


errorKeys :: [Text]
errorKeys = ["errorMessage", "reason", "errorReason", "error"]


codeKeys :: [Text]
codeKeys = ["errorCode", "serverErrorCode"]
