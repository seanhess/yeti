module Juniper.Component where

import Juniper.Prelude
import Data.Aeson (ToJSON, toJSON, encode)
import Lucid (Html, div_, class_, Attribute)
import Lucid.Base (makeAttribute)


-- TODO: ok, the goal is to provide some loose dynamic functions, and then we tighten them up when we create a component
-- so we don't have a 'component' function, but some helpers here.


-- component :: ToJSON input => [Attribute] -> Text -> input -> Html ()
-- component atts name inp =
--   div_ ([class_ name, class_ " ", makeAttribute "data-input" (cs $ encode inp)] <> atts) $ pure ()

component :: Text -> Attribute
component n = class_ (" " <> n)

dataInput :: ToJSON a => a -> Attribute
dataInput a = makeAttribute "data-input" (cs $ encode a)