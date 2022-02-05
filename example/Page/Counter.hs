{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Page.Counter where


import Juniper

import Data.String.Conversions (cs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5



type Params = (Integer, Maybe Text)

data Model = Model
  { count :: Integer
  , timestamp :: UTCTime
  , message :: Maybe Text
  } deriving (Show, Eq)

data Action
  = Increment
  | Decrement
  | Set Integer
  | GetTime
  deriving (Show, Read)
instance PageAction Action


params :: Model -> Params
params m = (m.count, m.message)

reload :: MonadIO m => Maybe Params -> m Model
reload ps = do
  let (c, msg) = fromMaybe (0, Nothing) ps
  t <- liftIO $ Time.getCurrentTime
  pure $ Model c t msg

update :: MonadIO m => Action -> Model -> m Model
update Increment m = pure $ m { count = count m + 1 }
update Decrement m = pure $ m { count = count m - 1 }
update (Set n)   m = pure $ m { count = n }

-- just refreshes the time, example of db update action, doesn't change state
update GetTime   m = pure m


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
page = Page params reload update view
