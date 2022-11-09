{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Focus where


import Prelude
import Yeti
import Yeti.UI

import Data.Text (Text)
import Lucid (toHtml)
import Lucid.Html5


data Model = Model
  { one :: Text
  , two :: Text
  } deriving (Show, Generic, LiveModel, Eq, ToJSON, FromJSON)

data Action
  = One Text
  | Two Text
  deriving (Show, Generic, LiveAction)


page :: Applicative m => Page () Model Action m
page = simplePage load update view

load :: Applicative m => m Model
load = do
  pure $ Model "a" "b"

update :: Applicative m => Action -> Model -> m Model
update (One t) m = pure $ m { one = t }
update (Two t) m = pure $ m { two = t }

view :: Model -> View Content ()
view m = section_ [ class_ "page" ] $ do
  "FOCUS: !"
  div_ [ class_ "section" ] $ do
    -- Inputs are committed on blur or enter
    div_ $ input' [ value_ m.one, onInput One ] ""
    div_ $ input' [ value_ m.two, onInput Two ] ""
    div_ $ toHtml $ m.one <> " " <> m.two



