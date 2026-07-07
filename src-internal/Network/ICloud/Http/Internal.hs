{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Network.ICloud.Http.Internal
  ( validateSetupBody
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap (fromList)
import Data.Text (Text)
import Network.ICloud.Trust.Internal (Setup2SADevice (..))


validateSetupBody :: Setup2SADevice -> Text -> Value
validateSetupBody (Setup2SADevice fields) code =
  Object $ fields <> fromList [("verificationCode", String code), ("trustBrowser", Bool True)]
