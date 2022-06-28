{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Juniper.Param2 where

import Data.String.Conversions (cs, ConvertibleStrings)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Function ((&))
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.ByteString.Lazy (ByteString)
-- import Data.Aeson (ToJSON, FromJSON)
-- import qualified Data.Aeson as Aeson
import Text.Megaparsec (runParser, Parsec, many, (<|>))
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)


type Error = Text
type Parser = Parsec Error Text

-- TODO use generics directly instead of JSON
-- TODO use megaparsec instead of this
class Param a where
  encode :: a -> Text
  parse :: Parser a

  -- default encode :: ToJSON a => a -> Text
  -- encode = cs . Aeson.encode

  -- default decode :: FromJSON a => Text -> Either Text a
  -- decode t =
  --   case Aeson.eitherDecode (cs t) of
  --     Left e -> Left $ cs e
  --     Right a -> Right a


instance Param Integer where
  encode n = cs $ show n
  parse = decimal

instance Param Int where
  encode n = cs $ show n
  parse = decimal

instance Param Bool where
  encode b = cs $ show b
  parse = bool

-- it should consume anything but a "/"
instance Param Text where
  encode t = t
  parse = undefined



instance Param Day where
  encode = cs . formatTime defaultTimeLocale "%Y-%m-%d"
  -- decode t =
  --   maybe (Left $ "could not parse day: " <> t) Right $ do
  --     parseTimeM True defaultTimeLocale "%Y-%m-%d" $ cs t

instance Param () where
  encode _ = ""
  -- decode _ = Right ()


bool :: Parser Bool
bool = do
  (const True <$> string "True") <|> (const False <$> string "False")



-- list2 :: (Param a, Param b) => [Text] -> Either Text (a, b)
-- list2 [at, bt] = do
--   a <- decode at
--   b <- decode bt
--   pure $ (a, b)
-- list2 ts = Left $ "Expected 2 segments, but got: " <> Text.intercalate "/" ts

-- list3 :: (Param a, Param b) => [Text] -> Either Text (a, b)
-- list3 [at, bt] = do
--   a <- decode at
--   b <- decode bt
--   pure $ (a, b)
-- list3 ts = Left $ "Expected 2 segments, but got: " <> Text.intercalate "/" ts

instance (Param a, Param b) => Param (a, b) where
  encode (a, b) = Text.intercalate "/" [ encode a,  encode b ]

  -- decode t =
  --   fromList $ Text.splitOn "/" t
  --   where

-- instance (ToJSON a, ToJSON b, ToJSON c, FromJSON a, FromJSON b, FromJSON c) => Param (a, b, c)

-- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON a, FromJSON b, FromJSON c, FromJSON d) => Param (a, b, c, d)



