module Juniper.Component where

import Juniper.Prelude
import Data.Aeson (ToJSON, toJSON, encode)
import Lucid (Html, div_, class_, Attribute)
import Lucid.Base (makeAttribute)

component :: ToJSON input => [Attribute] -> Text -> input -> Html ()
component atts name inp =
  div_ ([class_ name, class_ " ", makeAttribute "data-input" (cs $ encode inp)] <> atts) $ pure ()