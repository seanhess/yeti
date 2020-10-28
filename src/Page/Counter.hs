{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Counter where



import Wookie.Page
import Wookie.Events (click)

import Data.Map as Map (Map, fromList, lookup)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, defaultOptions, Options(sumEncoding), SumEncoding(..), Value(..))

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Control.Monad.State.Lazy (StateT)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Base (makeAttribute, Attribute)
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
  { _count :: Integer
  , _timestamp :: UTCTime
  , _message :: Maybe Text
  } deriving (Show, Eq)

makeLenses ''Model





-- instance Page Model Params where
--    loadPage = load
--    toParams = params


params :: Model -> Params
params (Model c _ msg) = (c, msg)




load :: MonadIO m => (Integer, Maybe Text) -> m Model
load (c, msg) = do
  t <- liftIO $ Time.getCurrentTime
  pure $ Model c t msg




-- does it have to be IO?
-- no.... it should be any MonadIO
update :: MonadIO m => Action -> StateT Model m ()
update Increment = count += 1
update Decrement = count -= 1
update (Set n) = count .= n



-- it will then get replaced with EXACTLY what's in that resolver
-- what about nested resolvers?
-- TODO make your own onclick attribute that accepts an Action, not text
view :: Model -> Html ()
view m = div_ $ do

  div_ $ do
    button_ [ click Increment] "Increment"
    button_ [ click Decrement] "Decrement"
    button_ [ click (Set 5)] "Set 5"

    p_ $ do
      span_ (toHtml $ fromMaybe "" $ m ^. message)

    p_ $ do
      span_ "Count: "
      span_ (toHtml $ show $ m ^. count)

    p_ $ do
      span_ "Time: "
      span_ (toHtml $ show $ m ^. timestamp)

    -- TODO function to generate links based on params
    p_ $ a_ [href_ "/app/counter?count=100"] "Click here to jump to Count = 100"
    p_ $ a_ [href_ "https://www.google.com"] "Google.com"
    p_ $ a_ [href_ "/app/about"] "About"








page :: MonadIO m => Page Params Model Action m
page = Page params load update view
