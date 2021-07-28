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
import Control.Monad.State.Lazy (StateT)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5



-- 





-- TODO make your own serialization, rather than Show / Read?
data Action
  = Increment
  | Decrement
  | Set Integer
  | Check Bool
  deriving (Show, Read)
instance PageAction Action


type Params = (Integer, Maybe Text, Bool)

data Model = Model
  { _count :: Integer
  , _timestamp :: UTCTime
  , _message :: Maybe Text
  , _checked :: Bool
  } deriving (Show, Eq)

makeLenses ''Model





-- instance Page Model Params where
--    loadPage = load
--    toParams = params


params :: Model -> Params
params (Model c _ msg ck) = (c, msg, ck)




load :: MonadIO m => Maybe Params -> m Model
load ps = do
  let (c, msg, ck) = fromMaybe (0, Nothing, False) ps
  t <- liftIO $ Time.getCurrentTime
  pure $ Model c t msg ck




-- does it have to be IO?
-- no.... it should be any MonadIO
update :: MonadIO m => Action -> StateT Model m ()
update Increment = count += 1
update Decrement = count -= 1
update (Set n)   = count .= n
update (Check b) = checked .= b



-- it will then get replaced with EXACTLY what's in that resolver
-- what about nested resolvers?
-- TODO make your own onclick attribute that accepts an Action, not text
view :: Model -> Html ()
view m = section_ [ class_ "page" ] $ do

    div_ [ class_ "section" ] $ do
      button_ [ onClick Increment] "Increment"
      button_ [ onClick Decrement] "Decrement"
      button_ [ onClick (Set 5)] "Set 5"

    -- see if I can get react to replace this
    div_ [ class_ "section" ] $ do
      span_ (toHtml $ fromMaybe "" $ m ^. message)

    div_ [ class_ "section" ] $ do
      span_ "Count: "
      span_ (toHtml $ show $ m ^. count)

    div_ [ class_ "section" ] $ do
      span_ "Time: "
      span_ (toHtml $ show $ m ^. timestamp)









page :: MonadIO m => Page Params Model Action m
page = Page params load update view
