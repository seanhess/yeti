{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Counter where



import Wookie.Runtime
import Wookie.Router

import Data.Map as Map (Map, fromList, lookup)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, defaultOptions, Options(sumEncoding), SumEncoding(..), Value(..))

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Text.Read (readMaybe)
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


-- My Component --------------------

-- TODO make your own serialization, rather than Show / Read?
data Action
  = Increment
  | Decrement
  | Set Integer
  deriving (Show, Read)
instance PageAction Action


-- can we override this?
class PageAction a where
  serialize :: a -> Text
  default serialize :: Show a => a -> Text
  serialize = cs . show

  -- class Enum a where
  --   enum :: [a]
  --   default enum :: (Generic a, GEnum (Rep a)) => [a]
  --   enum = map to genum

-- -- well, let's see, it can call it via the whole url!
-- -- we need to fix this
-- serializeAction :: Show action => action -> Text
-- serializeAction a = "runtime('"<> (cs $ show a) <> "')"



data Model = Model
  { _count :: Integer
  , _timestamp :: UTCTime
  } deriving (Show, Eq)

makeLenses ''Model




instance Page Model where
   toSegment (Model c _) = cs $ show c

   loadPage t = do
     -- TODO parse to a custom format. Map, Num, String, List
     -- it'll usually be multiple items, so default to map?
     -- or ALWAYS do a map. Yeah that makes sense.
     -- this already exists, it's called querystring
     -- OH! And the querystring behaves differently. It's not part of relative paths
     -- we definitely want to use it.
     -- Plus we'll have a library
     -- /app/counter/count:99
     -- /app/counter/left:3|right:9
     -- /app/counter?count=99
     -- /app/counter?left=99
     -- /app/counter?items=2,3,4&henry=dog%20tired
     -- yes, definitely. It's how PHP used to work
     Just n <- pure $ readMaybe (cs t)
     load n






load :: MonadIO m => Integer -> m Model
load c = do
  t <- liftIO $ Time.getCurrentTime
  pure $ Model c t




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
      span_ "Count: "
      span_ (toHtml $ show $ m ^. count)

    p_ $ do
      span_ "Time: "
      span_ (toHtml $ show $ m ^. timestamp)

    p_ $ a_ [href_ "/app/counter/100"] "Click here to jump to Count = 100"
    p_ $ a_ [href_ "https://www.google.com"] "Google.com"
    p_ $ a_ [href_ "/app/about"] "About"




-- our onclick attribute, but fancified
-- TODO better typeclass for action
-- we need to set TWO attributes
-- oh, no we don't
-- data-wookie-click="Action"
click :: PageAction action => action -> Attribute
click = makeAttribute "data-click" . serialize




