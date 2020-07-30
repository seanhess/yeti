{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Counter where



import Wookie.Runtime
import Wookie.Router

import Data.Map as Map (Map, fromList, lookup)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, defaultOptions, Options(sumEncoding), SumEncoding(..), Value(..))

import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Data.String.Conversions (cs)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Control.Monad.State.Lazy (StateT)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5


-- TESTS
-- TODO Pre-render the first page load - complete
-- TODO two counters
-- TODO serialize fragments. Map fragments to specific sub-components. Route Actions to those components.
-- TODO VDOM rendering


-- My Component --------------------

-- TODO make your own serialization, rather than Show / Read?
data Action
  = Increment
  | Decrement
  | Set Integer
  deriving (Show, Read)



data Model = Model
  { _count :: Integer
  , _timestamp :: UTCTime
  } deriving (Show, Eq)

makeLenses ''Model



-- we can't give a specific load method here
-- except we know the params for this match!
-- it could take Param or Route or something....





-- Load a model based on URL parameters
-- this shouldn't even be called if the URL is invalid. That's a 404 instead. _Router_
type Load = Integer -> IO Model
type Update = Action -> StateT Model IO ()
type View = Model -> Html ()


load :: Integer -> IO Model
load c = do
  t <- Time.getCurrentTime
  pure $ Model c t


-- Wait, so: Increment comes in. We have the URL, which contains
-- APPROACH 1 - pass the state up to the server every time.
-- APPROACH 2 - "reload" the whole page, but diff everything
-- Can the action *decide* what to do?


-- "RELOAD"
-- 1. load() from URL -> model
-- 2. call update with the model -> model'
-- 3. call view with model' -> Html



-- LINKS
-- /courses/1234/contents to /courses/12345/comments
-- Intercept the URL change
-- Send it as a special "load" action to the runtime
-- Get the root url, and diff, like normal. If parts are the same, they're the same
-- So the buttons could be implemented as links, if you wanted





-- instance ToSegment Model where
--   toSegment (Model m) = Num m
--   fromSegment (Num m) = pure $ Model m

--   -- an empty fragment? Not sure if this is the right thing to do or not
--   fromSegment (Path "") = pure $ Model 0
--   fromSegment v = fail $ "ToSegment: Expected Number, but got " <> show v


update :: Action -> StateT Model IO ()
update Increment = count += 1
update Decrement = count -= 1
update (Set n) = count .= n



-- it will then get replaced with EXACTLY what's in that resolver
-- what about nested resolvers?
-- TODO make your own onclick attribute that accepts an Action, not text
view :: Model -> Html ()
view m = div_ $ do

  div_ $ do
    h1_ "Counter"
    button_ [ onclick_ (serializeAction Increment)] "Increment"
    button_ [ onclick_ (serializeAction Decrement)] "Decrement"
    button_ [ onclick_ (serializeAction (Set 5))] "Set 5"

    p_ $ do
      span_ "Count: "
      span_ (toHtml $ show $ m ^. count)

    p_ $ do
      span_ "Time: "
      span_ (toHtml $ show $ m ^. timestamp)


-- well, let's see, it can call it via the whole url!
serializeAction :: Action -> Text
serializeAction a = "runtime('"<> (cs $ show a) <> "')"




