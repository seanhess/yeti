{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Counter where


import Wookie

import Data.String.Conversions (cs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5



data Action
  = Increment
  | Decrement
  | Set Integer
  | GetTime
  deriving (Show, Read)
instance PageAction Action

type Params = (Integer, Maybe Text)

data Model = Model
  { count :: Integer
  , timestamp :: UTCTime
  , message :: Maybe Text
  } deriving (Show, Eq)



params :: Model -> Params
params m = (m.count, m.message)


load :: MonadIO m => Maybe Params -> m Model
load ps = do
  let (c, msg) = fromMaybe (0, Nothing) ps
  t <- liftIO $ Time.getCurrentTime
  pure $ Model c t msg


-- does it have to be IO?
-- no.... it should be any MonadIO
update :: MonadIO m => Action -> Model -> m Model
update Increment m = pure $ m { count = count m + 1 }
update Decrement m = pure $ m { count = count m - 1 }
update (Set n)   m = pure $ m { count = n }

-- just refreshes the time, example of db update action, doesn't change state
update GetTime   m = pure m



-- it will then get replaced with EXACTLY what's in that resolver
-- what about nested resolvers?
-- TODO make your own onclick attribute that accepts an Action, not text
view :: Model -> Html ()
view m = section_ [ class_ "page" ] $ do

    div_ [ class_ "section" ] $ do
      button_ [ onClick Increment] "Increment"
      button_ [ onClick Decrement] "Decrement"
      button_ [ onClick (Set 5)] "Set 5"
      button_ [ onClick GetTime ] "Get Time"

    -- see if I can get react to replace this
    div_ [ class_ "section" ] $ do
      span_ (toHtml $ fromMaybe "" $ message m)

    div_ [ class_ "section" ] $ do
      span_ "Count: "
      span_ (toHtml $ show $ count m)

    div_ [ class_ "section" ] $ do
      span_ "Time: "
      span_ (toHtml $ show $ timestamp m)









page :: MonadIO m => Page Params Model Action m
page = Page params load update view
