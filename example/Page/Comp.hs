{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Comp where

import Prelude
import Juniper
import Data.Text (pack, unpack, Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_)
import Lucid (Html, toHtml)
import Lucid.Base (makeAttributes, Attributes)
import Lucid.Html5

data Model = Model
  { items :: [String]
  , count :: Int
  } deriving (Show, Generic, LiveModel)

data Action
  = AddItem
  | RemoveItem String
  | Test String
  | DoNothing String
  | Increment
  deriving (Show, Generic, LiveAction)

load :: MonadIO m => m Model
load = pure $ Model ["Empty"] 0

update :: MonadIO m => Action -> Model -> m Model

update AddItem m = do
  let n = length m.items
      i = "Item: " <> show n
  pure $ m { items = i:m.items }

update (RemoveItem i) m = do
  pure $ m { items = filter (/= i) m.items }

update Increment m = do
  pure $ m { count = m.count + 1 }

update (Test _) m = do
  pure $ m

update (DoNothing _) m = do
  pure $ m

view :: Model -> Html ()
view m = section_ [ class_ "g10 p10 col", id_ "parent" ] $ do

    button_ [ onClick Increment ] "DoSomething"

    div_ $ toHtml $ show m.count

    button_ [ onClick AddItem ] "AddItem"

    deleteList [class_ "bg-blue p12"] m.items RemoveItem

    deleteList [class_ "bg-green p12"] m.items DoNothing

    button_ [] "Button"

    div_ [id_ "items"] $ do
      forM_ m.items $ \item -> do
        div_ $ toHtml item


deleteList :: [Attributes] -> [String] -> (String -> Action) -> Html ()
deleteList as items act = 
  div_ ([component "comp", dataInput items, onValue "delete" (act . unpack)] <> as) $ pure ()


page :: MonadIO m => Page () Model Action m
page = simplePage load update view
