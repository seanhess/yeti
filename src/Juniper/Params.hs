{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Juniper.Params where

import Juniper.Prelude
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson



-- class HasParams m p | p -> m where
--   toParams :: m -> p

--   default toParams :: (p ~ ()) => m -> p
--   toParams _ = ()

--   defParams :: p
--   default defParams :: (p ~ ()) => p
--   defParams = ()

--   -- this shouldn't be the scotty type, but it's close, so why not?
--   encParams :: p -> [(LT.Text, LT.Text)]

--   default encParams :: p -> [(LT.Text, LT.Text)]
--   encParams _ = []

--   decParams :: [(LT.Text, LT.Text)] -> Maybe p

--   default decParams :: [(LT.Text, LT.Text)] -> Maybe p
--   decParams _ = Nothing



-- TODO use generics directly instead of JSON
-- does it always serialize to a querystring?
-- what if you wanted to use paths?
class ToParams p where

  encode :: p -> Text
  default encode :: p -> Text
  encode _ = ""

  decode :: Text -> Maybe p
  default decode :: Text -> Maybe p
  decode _ = Nothing

-- instance ToParams Day where
--   encode = cs . formatTime defaultTimeLocale "%Y-%m-%d"
--   decode = parseTimeM True defaultTimeLocale "%Y-%m-%d" . cs

instance ToParams ()

-- instance (ToJSON a, ToJSON b, FromJSON a, FromJSON b) => ToParams (a, b)
-- instance (ToJSON a, ToJSON b, ToJSON c, FromJSON a, FromJSON b, FromJSON c) => ToParams (a, b, c)
-- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON a, FromJSON b, FromJSON c, FromJSON d) => ToParams (a, b, c, d)



