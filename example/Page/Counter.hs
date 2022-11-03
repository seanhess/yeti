{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Counter where

import Prelude
import Data.Text (pack)
import Yeti
import Yeti.View.UI
import Control.Monad.IO.Class (MonadIO)
-- import Lucid (toHtml)
-- import Lucid.Html5

data Model = Model
  { count :: Integer
  } deriving (Show, Generic, LiveModel, ToJSON, FromJSON)

data Params = Params
  { _count :: Integer
  } deriving (Show, Generic, ToParams)

params :: Model -> Params
params (Model n) = Params n

data Action
  = Increment
  | Decrement
  deriving (Show, Generic, LiveAction)


-- just run these in your monad
-- oh, this doesn't want to worry about which monad it is run in
load :: MonadIO m => Integer -> (Maybe Params) -> m Model
load _ (Just (Params n)) = pure $ Model n
load n _ = pure $ Model n

update :: MonadIO m => Action -> Model -> m Model
update Increment m = pure $ m { count = m.count + 1 }
update Decrement m = pure $ m { count = m.count - 1 }

view :: Model -> View Content ()
view m = col (p S1) $ do

    row (p S1) $ do
      button Decrement id "Decrement"
      button Increment id "Increment"

    row (p S1) $ do
      tag "input" id ""

    txt (p S1) $ pack $ show m.count




page :: MonadIO m => Integer -> Page Params Model Action m
page n = Page params (load n) update view


-- instance MonadIO m => SPage Model m where
--   type Params Model = ()
--   type Msg Model = Action
--   load' = load
--   params' = const ()
--   update' = update
--   view' = view

  -- type Msg Model = Action
  -- load' = load
  -- update' = update
  -- view' = view
  -- params' = const ()