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
class ToParams a where
  encode :: a -> Text
  decode :: Text -> Maybe a

  default encode :: Show a => a -> Text
  encode = cs . show

  default decode :: Read a => Text -> Maybe a
  decode = readMaybe . cs

instance ToParams Integer
instance ToParams Int
instance ToParams Bool

instance ToParams Day where
  encode = cs . formatTime defaultTimeLocale "%Y-%m-%d"
  decode = parseTimeM True defaultTimeLocale "%Y-%m-%d" . cs

instance ToParams () where
  encode _ = ""
  decode _ = Just ()

instance (Show a, Show b, Read a, Read b) => ToParams (a, b)

instance (Show a, Show b, Show c, Read a, Read b, Read c) => ToParams (a, b, c)

instance (Show a, Show b, Show c, Show d, Read a, Read b, Read c, Read d) => ToParams (a, b, c, d)

-- this will decode Maybe Text as ""
-- instance (ToParams a, Show a, Read a) => ToParams (Maybe a)


