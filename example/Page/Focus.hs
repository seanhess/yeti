{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
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



data Action
  = One Value
  | Two Value
  deriving (Show, Read)
instance PageAction Action

data Model = Model
  { one :: Text
  , two :: Text
  } deriving (Show, Read, Eq, ToParams)




load :: MonadIO m => Maybe Model -> m Model
load (Just m) = pure m
load Nothing = do
  pure $ Model "a" "b"


-- does it have to be IO?
-- no.... it should be any MonadIO
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










page :: MonadIO m => Page Model Model Action m
page = Page id load update view


