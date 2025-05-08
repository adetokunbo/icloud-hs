{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Network.ICloud.Http.CookieJar
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Support the loading and saving of cookies from a standard cookie file format
-}
module Network.ICloud.Http.CookieJar (usingJarCookies) where

import Data.Attoparsec.Cookie (readJar, writeNetscapeJar)
import Data.Time (getCurrentTime)
import Network.HTTP.Client
  ( Request (..)
  , Response (..)
  , createCookieJar
  , insertCookiesIntoRequest
  , updateCookieJar
  )
import System.Directory (doesFileExist)


{- |
if the cookie jar file exists
then
  load it.
  update the cookie jar from the request and response
  save it
else
  ensure its parent directory exists
  create the cookie jar from the request and response
  save it

currently unhandled:
  cannot create directory
  cannot write due to permissions
  files exists, but data cannot be parsed
-}
updateCookieJarOf' :: FilePath -> Response a -> Request -> IO (Response a)
updateCookieJarOf' dataPath resp req = do
  pathExists <- doesFileExist dataPath
  now <- getCurrentTime
  if pathExists
    then do
      readJar dataPath >>= \case
        Left e -> fail $ show e
        Right old -> do
          let (updated, resp_) = updateCookieJar resp req now old
          writeNetscapeJar dataPath updated
          pure resp_
    else do
      let (updated, resp_) = updateCookieJar resp req now $ createCookieJar []
      writeNetscapeJar dataPath updated
      pure resp_


addCookiesFromJar :: FilePath -> Request -> IO Request
addCookiesFromJar dataPath req = do
  pathExists <- doesFileExist dataPath
  if not pathExists
    then pure req
    else do
      now <- getCurrentTime
      readJar dataPath >>= \case
        Left e -> fail $ show e
        Right jar -> do
          let (req', jar') = insertCookiesIntoRequest req jar now
          writeNetscapeJar dataPath jar'
          pure req'


usingJarCookies :: FilePath -> Request -> (Request -> IO (Response b)) -> IO (Response b)
usingJarCookies cookieJarPath req doReq = do
  req' <- addCookiesFromJar cookieJarPath req
  resp <- doReq req'
  updateCookieJarOf' cookieJarPath resp req'
