{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Counter where

import Prelude
import Juniper
import Control.Monad.IO.Class (MonadIO)
import Lucid (Html, toHtml)
import Lucid.Html5

data Model = Model
  { count :: Integer
  } deriving (Generic, ToJSON, FromJSON, ToParams)

instance HasParams Model () where
  toParams _ = ()
  defParams = ()

data Action
  = Increment
  | Decrement
  deriving (Show, Read, PageAction)


load :: MonadIO m => m Model
load = pure $ Model 0

update :: MonadIO m => Action -> Model -> m Model
update Increment m = pure $ m { count = count m + 1 }
update Decrement m = pure $ m { count = count m - 1 }

view :: Model -> Html ()
view m = section_ [ class_ "page" ] $ do

    div_ [ class_ "section" ] $ do
      button_ [ onClick Increment] "Increment"
      button_ [ onClick Decrement] "Decrement"


page :: MonadIO m => Page () Model Action m
page = simplePage load update view
