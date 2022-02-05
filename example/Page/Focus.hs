{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Page.Focus where


import Juniper

import Data.String.Conversions (cs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5


data Model = Model
  { one :: Text
  , two :: Text
  } deriving (Show, Read, Eq, ToParams)

data Action
  = One Value
  | Two Value
  deriving (Show, Read, PageAction)


page :: MonadIO m => Page Model Model Action m
page = simplePage load update view

load :: MonadIO m => m Model
load = do
  pure $ Model "a" "b"

update :: MonadIO m => Action -> Model -> m Model
update (One (Value t)) m = pure $ m { one = t }
update (Two (Value t)) m = pure $ m { two = t }

view :: Model -> Html ()
view m = section_ [ class_ "page" ] $ do
    div_ [ class_ "section" ] $ do
      -- Inputs are committed on blur or enter
      div_ $ input_ [ value_ m.one, onInput One ]
      div_ $ input_ [ value_ m.two, onInput Two ]
      div_ $ toHtml $ m.one <> " " <> m.two



