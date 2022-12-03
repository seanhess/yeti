{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Counter where

import Prelude
import Data.Text (pack, Text)
import Yeti
import Yeti.UI
import Yeti.View.Types (Tag(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Char (toLower)
import qualified Data.Text as Text
import App.Color
-- import Lucid (toHtml)
-- import Lucid.Html5

data Model = Model
  { count :: Integer
  } deriving (Show, Generic, LiveModel, ToJSON, FromJSON)

data Params = Params
  { pcount :: Integer
  } deriving (Show, Generic, ToParams)

params :: Model -> Params
params (Model n) = Params n

data Action
  = Increment
  | Decrement
  | Noop Text
  deriving (Show, Generic, LiveAction)


-- just run these in your monad
-- oh, this doesn't want to worry about which monad it is run in
load :: MonadIO m => Integer -> (Maybe Params) -> m Model
load _ (Just (Params n)) = pure $ Model n
load n _ = pure $ Model n


-- it's not an actual state monad
-- it's something else

update :: MonadIO m => Action -> Model -> m Model

update Increment m = do
  pure $ m { count = m.count + 1 }

update Decrement m = do
  pure $ m { count = m.count - 1 }

update _ m = pure m

view :: Model -> View Content ()
view m = col (gap 8) $ do

    row (gap 10) $ do
      btn Decrement "Decrement"
      btn Increment "Increment"

      -- <button class="shadow bg-purple-500 hover:bg-purple-400 focus:shadow-outline focus:outline-none text-white font-bold py-2 px-4 rounded" type="button">
      --   Sign Up
      -- </button>

    -- row (pad 1 . gap 6) $ do
    --   input Noop id

    -- example: conditionally include a style
    text (if m.count > 10 then bold else id) $
      pack $ show m.count

  where
    -- btn act = button act (px S4 . py S2 rounded)

    btn act = button act (shadow . padX 8 . padY 4 . bg Purple . hover |: bg PurpleLight . color White)


    -- btn act = button act (hover |: bg Green)

    






page :: MonadIO m => Integer -> Page Params Model Action m
page n = Page params (load n) update view
