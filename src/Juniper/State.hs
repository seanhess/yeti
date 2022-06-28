module Juniper.State where

import Juniper.Prelude
import Text.Read (readMaybe)

class ToState model where
  encode :: model -> Text
  decode :: Text -> Maybe model

  default encode :: Show model => model -> Text
  encode m = cs $ show m

  default decode :: Read model => Text -> Maybe model
  decode t = readMaybe $ cs t
