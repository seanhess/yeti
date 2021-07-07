{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Counter where


import Wookie.Page as Page
import Wookie.Events (click)

import Data.String.Conversions (cs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Monad.State.Lazy (StateT)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5 hiding (onclick_)



-- 



-- TESTS
-- TODO Pre-render the first page load - complete
-- TODO two counters
-- TODO serialize fragments. Map fragments to specific sub-components. Route Actions to those components.
-- TODO VDOM rendering



-- TODO make your own serialization, rather than Show / Read?
data Action
  = Increment
  | Decrement
  | Set Integer
  deriving (Show, Read)
instance PageAction Action


type Params = (Integer, Maybe Text)

data Model = Model
  { count :: Integer
  , timestamp :: UTCTime
  , message :: Maybe Text
  } deriving (Show, Eq)





-- instance Page Model Params where
--    loadPage = load
--    toParams = params


params :: Model -> Params
params (Model c _ msg) = (c, msg)




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



-- it will then get replaced with EXACTLY what's in that resolver
-- what about nested resolvers?
-- TODO make your own onclick attribute that accepts an Action, not text
view :: Model -> Html ()
view m = section_ $ do

    button_ [ click Increment] "Increment"
    button_ [ click Decrement] "Decrement"
    button_ [ click (Set 5)] "Set 5"

    -- see if I can get react to replace this
    p_ $ do
      span_ (toHtml $ fromMaybe "" $ message m)

    p_ [class_ ("message " <> (cs $ show $ count m))] $ do
      span_ "Count: "
      span_ (toHtml $ show $ count m)

    p_ $ do
      span_ "Time: "
      span_ (toHtml $ show $ timestamp m)

    h3_ "Test Form"
    div_ $ 
      form_ $ input_ [ type_ "text", name_ "test" ]

    -- -- TODO function to generate links based on params
    -- p_ $ a_ [href_ "/app/counter?count=100"] "Click here to jump to Count = 100"
    -- p_ $ a_ [click (Set 15), style_ "text-decoration:underline;cursor:pointer"] "Click here to jump to Count = 15"
    -- p_ $ a_ [href_ "https://www.google.com"] "Google.com"
    -- p_ $ a_ [href_ "/app/about"] "About"








page :: MonadIO m => Page Params Model Action m
page = Page params load update view
