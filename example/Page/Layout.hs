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

  row [ width Fill, padding 10 ] $ do
    -- yeah, you specify another set of attributes for "hover"
    button [ background "red" ] $ do
      "<"
    space
    button [] ">"

  column [ width Fill, padding 10 ] $ do
    "content"

  space

  row [ width Fill, padding 10, background "RoyalBlue", color "white" ] $ do
    "tray"
  

--   column [ background "blue", height Fill, width Fill ] $ do
--     row [ ] "One"
--     row [ background (Color "yellow") ] "Two"
--     row [ ] "Three"
--     row [ height Fill, width Fill ] $ do
--       space
--       el [ background "red", height Fill ] "Space (Wide)"
--     row [ width Fill, background (Color "green") ] $ do
--       el [ height (Px 30) ] "start"
--       el [ height Fill, width Fill, background "AA0000FF" ] "space"
--       el "end"
--     row [ background "orange", width Fill ] $ do
--       space
--       el "center"
--       space
--     -- el [ background "red" ] "umm"
--     -- el [ background "green", width (Px 100) ] "two"

--     -- row $ do
--     --   el [ width Fill, background "yellow" ] "yL"
--     --   el [ background "gray" ] "hi"
--   -- replicateM_ 200 $ p_ "HI"

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
