{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Wookie.Params where

import Data.String.Conversions (cs, ConvertibleStrings)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Function ((&))
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)



-- TODO use megaparsec
class Params a where
  encode :: a -> Text
  decode :: Text -> Maybe a

  default encode :: Show a => a -> Text
  encode = cs . show

  default decode :: Read a => Text -> Maybe a
  decode = readMaybe . cs


instance Params Integer
instance Params Int
instance Params Bool

instance Params Day where
  encode = cs . formatTime defaultTimeLocale "%Y-%m-%d"
  decode = parseTimeM True defaultTimeLocale "%Y-%m-%d" . cs

instance Params Text where
  encode = id
  decode = Just . id

instance Params [Char] where
  encode = cs
  decode = Just . cs

instance Params () where
  encode _ = ""
  decode _ = Just ()

-- this will decode Maybe Text as ""
instance Params a => Params (Maybe a) where
  encode (Just a) = encode a
  encode Nothing = ""

  decode "" = Just $ Nothing
  decode a = Just $ decode a

instance (Params a, Params b) => Params (a, b) where
  encode (a, b) = Text.intercalate ":" $ [encode a, encode b]
  decode t = do
    [at, bt] <- pure $ Text.splitOn ":" t
    a <- decode at
    b <- decode bt
    pure (a, b)

instance (Params a, Params b, Params c) => Params (a, b, c) where
  encode (a, b, c) = Text.intercalate ":" $ [encode a, encode b, encode c]
  decode t = do
    [at, bt, ct] <- pure $ Text.splitOn ":" t
    a <- decode at
    b <- decode bt
    c <- decode ct
    pure (a, b, c)

instance (Params a, Params b, Params c, Params d) => Params (a, b, c, d) where
  encode (a, b, c, d) = Text.intercalate ":" $ [encode a, encode b, encode c, encode d]
  decode t = do
    [at, bt, ct, dt] <- pure $ Text.splitOn ":" t
    a <- decode at
    b <- decode bt
    c <- decode ct
    d <- decode dt
    pure (a, b, c, d)

instance (Params a, Params b, Params c, Params d, Params e) => Params (a, b, c, d, e) where
  encode (a, b, c, d, e) = Text.intercalate ":" $ [encode a, encode b, encode c, encode d, encode e]
  decode t = do
    [at, bt, ct, dt, et] <- pure $ Text.splitOn ":" t
    a <- decode at
    b <- decode bt
    c <- decode ct
    d <- decode dt
    e <- decode et
    pure (a, b, c, d, e)

instance Params a => Params [a] where
  encode [] = "[]"
  encode as = "[" <> Text.intercalate "," (fmap encode as) <> "]"

  -- they MUST have [ and ] around them?
  -- maybe I should use a real parser...
  decode "[]" = Just []
  decode t = Just $ t
    & Text.dropAround isBracket
    & Text.splitOn ","
    & mapMaybe decode
    where isBracket c = c == '[' || c == ']'
