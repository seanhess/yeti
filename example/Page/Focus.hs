{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Focus where


import Prelude
import Juniper
import Sockets

import Data.String.Conversions (cs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Lucid (Html, toHtml, toHtmlRaw, renderBS, Term, term)
import Lucid.Html5


data Model = Model
  { one :: Text
  , two :: Text
  } deriving (Show, Generic, LiveModel, Eq, ToJSON, FromJSON)

data Action
  = One Text
  | Two Text
  deriving (Show, Generic, LiveAction)


page :: MonadIO m => Page () Model Action m
page = simplePage load update view

load :: MonadIO m => m Model
load = do
  pure $ Model "a" "b"

update :: MonadIO m => Action -> Model -> m Model
update (One t) m = pure $ m { one = t }
update (Two t) m = pure $ m { two = t }

view :: Model -> Html ()
view m = section_ [ class_ "page" ] $ do
  "FOCUS: !"
  div_ [ class_ "section" ] $ do
    -- Inputs are committed on blur or enter
    div_ $ input' [ value_ m.one, onInput One ] ""
    div_ $ input' [ value_ m.two, onInput Two ] ""
    div_ $ toHtml $ m.one <> " " <> m.two


input' :: Term arg result => arg -> result
input' = term "input"


