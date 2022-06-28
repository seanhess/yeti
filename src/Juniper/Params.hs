{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Juniper.Params where

import Juniper.Prelude
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson

import qualified Data.Text.Lazy as LT



class HasParams m p | p -> m where
  toParams :: m -> p

  defParams :: p

  -- this shouldn't be the scotty type, but it's close, so why not?
  encParams :: p -> [(LT.Text, LT.Text)]

  default encParams :: p -> [(LT.Text, LT.Text)]
  encParams _ = []

  decParams :: [(LT.Text, LT.Text)] -> Maybe p

  default decParams :: [(LT.Text, LT.Text)] -> Maybe p
  decParams _ = Nothing




-- TODO use generics directly instead of JSON
class ToParams a where
  encode :: a -> Text
  decode :: Text -> Maybe a

  default encode :: ToJSON a => a -> Text
  encode = cs . Aeson.encode

  default decode :: FromJSON a => Text -> Maybe a
  decode = Aeson.decode . cs

instance ToParams Integer
instance ToParams Int
instance ToParams Bool

instance ToParams Day where
  encode = cs . formatTime defaultTimeLocale "%Y-%m-%d"
  decode = parseTimeM True defaultTimeLocale "%Y-%m-%d" . cs

instance ToParams () where
  encode _ = ""
  decode _ = Just ()

instance (ToJSON a, ToJSON b, FromJSON a, FromJSON b) => ToParams (a, b)

instance (ToJSON a, ToJSON b, ToJSON c, FromJSON a, FromJSON b, FromJSON c) => ToParams (a, b, c)

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON a, FromJSON b, FromJSON c, FromJSON d) => ToParams (a, b, c, d)



