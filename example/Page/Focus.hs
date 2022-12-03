{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Focus where


import Prelude
import Yeti
import Yeti.UI

import Data.Text (Text)


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
view m = col (gap 10) $ do
  field (gap 4) LabelLeft "One" $ inputText One m.one id
  field (gap 4) LabelLeft "Two" $ inputText Two m.two id
  text_ $ m.one <> " " <> m.two



