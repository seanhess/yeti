{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Component where

import Prelude
import Juniper
import Data.Text (pack, Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_)
import Lucid (Html, toHtml)
import Lucid.Base (makeAttribute)
import Lucid.Html5

data Model = Model
  { items :: [String]
  , count :: Int
  } deriving (Read, Show, Encode LiveModel)

data Action
  = AddItem
  | Increment
  deriving (Show, Read, Encode LiveAction)

load :: MonadIO m => m Model
load = pure $ Model ["Empty"] 0

update :: MonadIO m => Action -> Model -> m Model
update AddItem m = do
  let n = length m.items
      i = "Item: " <> show n
  pure $ m { items = i:m.items }
update Increment m = do
  pure $ m { count = m.count + 1 }

view :: Model -> Html ()
view m = section_ [ class_ "g10 p10 col", id_ "parent" ] $ do

    button_ [ onClick Increment ] "DoSomething"

    div_ $ toHtml $ show m.count

    button_ [ onClick AddItem ] "AddItem"

    div_ [ class_ "bg-blue p12 comp", id_ "comp", makeAttribute "data-input" (pack $ show m.items) ] $ do
      div_ "Component Empty Content - should be replaced by component"

    div_ [id_ "items"] $ do
      forM_ m.items $ \item -> do
        div_ $ toHtml item



page :: MonadIO m => Page () Model Action m
page = simplePage load update view
