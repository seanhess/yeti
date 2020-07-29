{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Api where



import Runtime

import Data.Map as Map (Map, fromList)
import Data.Aeson (ToJSON(..), genericToJSON, defaultOptions, Options(sumEncoding), SumEncoding(..), Value(..))

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


toRoutePath :: ToJSON a => a -> Path
toRoutePath a = undefined $ toJSON a

-- What if it can't be mapped?
-- Maybe I should make it support everything
toRouteSegment :: Value -> Segment
toRouteSegment (String s) = Single (Str s)
toRouteSegment (Number n) = Single (Num $ round n)
toRouteSegment (Bool b) = Single (Flag b)
-- toRouteSegment (Array a) = Multi (map toRouteSegment a)
-- toRouteSegment (Object o) = Multi (map toRouteSegment a)

-- We don't want to fail parsing here
-- but we aren't in JSON land anymore
-- we could parse it manually though, using Parsec



class RoutePath a where
  toPath :: a -> Path
  -- parsePath :: Path -> Maybe a


data Route
    = Counter Integer
    | Course CourseId CoursePage
    | Courses CourseSearch
    deriving (Generic)
instance ToJSON Route where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }

data CoursePage = Contents | Comments
  deriving (Generic)
instance ToJSON CoursePage

data CourseSearch = CourseSearch
  { term :: Text
  , category :: Text
  } deriving (Generic)
instance ToJSON CourseSearch

-- Each of these should go to a segment, no?
-- could it do it from the aeson?
-- aeson doesn't map directly to a route though, because it can be infinitely nested

type CourseId = Text

-- could we resuse these definititions to provide two-way mappings?
-- polymorphism

-- these are when you are GIVEN a Counter c. I fail to see how you could make a parser from them

instance RoutePath Route where
  toPath (Counter c)      = Root </> "counter" </> param c
  toPath (Course i page) = Root </> "course" </> param i </> param page
  toPath (Courses search) = Root </> "courses" </> (fields ["term" .: term search, "category" .: category search])


  -- we need a more dynamic way to get this to work
  -- you can't use it here because it's not a constructor
  -- parsePath (Root :> "counter" :> (Single (Num c))) = Just $ Counter c
  -- parsePath Root </> "course" </> (Single (Str id)) </> (Single (Str page)) = 
  -- parsePath (Courses search) = Root </> "courses" </> (fields ["term" .: term search, "category" .: category search])


instance Param CoursePage where
  toFragment Contents = "contents"
  toFragment Comments = "comments"


-- genericFields

---- | A configurable generic JSON creator. This function applied to
---- 'defaultOptions' is used as the default for 'toJSON' when the type
---- is an instance of 'Generic'.
--genericToJSON :: (Generic a, GToJSON' Value Zero (Rep a))
--              => Options -> a -> Value
--genericToJSON opts = gToJSON opts NoToArgs . from

---- | Class of generic representation types that can be converted to
---- JSON.
--class GToJSON' enc arity f where
--    -- | This method (applied to 'defaultOptions') is used as the
--    -- default generic implementation of 'toJSON'
--    -- (with @enc ~ 'Value'@ and @arity ~ 'Zero'@)
--    -- and 'liftToJSON' (if the @arity@ is 'One').
--    --
--    -- It also provides a generic implementation of 'toEncoding'
--    -- (with @enc ~ 'Encoding'@ and @arity ~ 'Zero'@)
--    -- and 'liftToEncoding' (if the @arity@ is 'One').
--    gToJSON :: Options -> ToArgs enc arity a -> f a -> enc


-- instance RoutePath CourseSearch where
--   toPath (CourseSearch t c) = [Fields $ Map.fromList [("term" ,toParam t), ("category", toParam c)]]

-- instance RoutePath Text where
--   toPath t = [Path t]

-- instance RoutePath Integer where
--   toPath t = [Path (toParam t)]




-- /counter/:n
-- /twins/:n/:n
-- /course/:course-id/contents
-- /course/:course-id/comments
-- /courses/
-- /courses/term:aerospace
-- /twins2/left:n|(right:n)/

-- /courses/stuff:(left:5|right:10)



-- type CourseId

-- I mean, you can use Generic to figure it out automatically.
-- Ooh, yes please


-- class ToURL a where
--   toURL a =


-- It's not two-way
-- I need a way to tell them what the URL is
-- Hmmmmm
-- Maybe it would be better to have my own endpoint
-- because it has to go both ways
-- it could be servant though
-- do I want to it fail if I don't get it right?
-- No, because I'm not going to manually write URLs ever. I'll link to a specific page?
-- Link: Link (Counter 3)
-- yeah then it'll serialize
-- seems like the Elm method should work
-- just have it parse / serialize
-- but there's no good way to have it round trip exactly right
-- you're in route might not match your out route
-- you need a one-way mapping

-- I like the idea of the url being automatic. I don't want to "design" it

-- page = Page


-- parse this url!
-- route :: URL -> 
-- well, I could use the routing system of somebody else to do this. Like scotty's, no?
-- I mean, they're supposed to be POSTing directly to me
-- It wouldn't kill me to use something like that, instead of my own
-- just make it easy to bootstrap

-- runtime Api.view Api.update (Api.load p)


-- view, update, load
-- we'll need to delegate all of them, based on the route
-- unless we make a "Component" record with them



-- Load a model based on URL parameters
-- this shouldn't even be called if the URL is invalid. That's a 404 instead. _Router_
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




