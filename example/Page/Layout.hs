{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Layout where


import Wookie
import Wookie.UI

import Data.String.Conversions (cs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (replicateM_)
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
view m = layout [ background "white" ] $ do
  column [ background "blue", height Fill, width Fill ] $ do
    row [ ] "One"
    row [ ] "Two"
    row [ ] "Three"
    row [ height Fill, background "red" ] "Space is longer"
    row [ width Fill, background "green" ] $ do
      el "start"
      el [ width Fill ] "space"
      el "end"
    -- el [ background "red" ] "umm"
    -- el [ background "green", width (Px 100) ] "two"

    -- row $ do
    --   el [ width Fill, background "yellow" ] "yL"
    --   el [ background "gray" ] "hi"
  -- replicateM_ 200 $ p_ "HI"

    -- see if I can get react to replace this
    -- div_ [] $ do
    --   span_ (toHtml $ fromMaybe "" $ m ^. message)

    -- div_ [] $ do
    --   span_ "Count: "
    --   span_ (toHtml $ show $ m ^. count)

    -- div_ [] $ do
    --   span_ "Time: "
    --   span_ (toHtml $ show $ m ^. timestamp)

    -- row [ height Fill, background "blue" ] "hello"

    -- row [ spacing 5, background "red" ] $ do
    --   button_ [ onClick Increment] "Increment"
    --   button_ [ onClick Decrement] "Decrement"
    --   el [ width Fill ] "EL"
    --   button_ [ onClick (Set 5)] "Set 5"











page :: MonadIO m => Page Params Model Action m
page = Page params load update view
