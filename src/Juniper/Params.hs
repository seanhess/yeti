{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Juniper.Params where

import Data.String.Conversions (cs, ConvertibleStrings)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Function ((&))
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson



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



