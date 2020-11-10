{-# LANGUAGE OverloadedStrings #-}
module Wookie.Params where

import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Function ((&))
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)



-- TODO use megaparsec
class Params a where
  encode :: a -> Text
  decode :: Text -> Maybe a

instance Params Integer where
  encode = cs . show
  decode = readMaybe . cs

instance Params Int where
  encode = cs . show
  decode = readMaybe . cs

instance Params Text where
  encode = id
  decode = Just . id

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
