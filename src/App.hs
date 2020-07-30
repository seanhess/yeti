{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module App where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
-- import Counter
import Wookie.Router
import Wookie.Runtime
import Lucid (Html)







-- resolve :: Text -> Maybe (ByteString -> IO Response)
-- resolve t = do
--   r <- fromPath =<< parsePath t
--   pure $ resolveRoute r


-- This gets the new model, doesn't update the route!
-- resolveRoute :: Route -> (ByteString -> IO Response)
-- resolveRoute (Counter n) = runAction (Counter.load n) Counter.update Counter.view




-- What are we targeting?
-- /twins/left:5|right:4
-- counter doesn't know it's route is /counter/3. it might be the above!
-- how would it know it needs to update the LEFT num?
-- and how does it update the url?

-- We need Model -> Route FOR SURE, but on the page? Or in here?
-- We have Route -> Model here, in resolve route

-- Click: URL = left:5
-- it's not going to be obvious how to map that
-- POST /twins/left:5 Increment
-- but it's *possible*. I just need to "name" them somehow. Very different approach
-- 1. Find Counter. Call with left:5
-- 2. Update -> View
-- 3. New URL: /twins/left:6
-- ..... but it doesn't know how to just update THAT part of the url
-- re-render the WHOLE page
-- re-render the WHOLE url
-- did we switch pages? Naw.
-- wait. We're on this page
-- Twin: has counter + counter
-- we call increment on the left one
-- sub-pages aren't going to make ANY sense
-- Twin could definitely do (Increment Left) or (Increment Right)
-- Do we really want nested pages?? When woudl I ever use this?
-- Routing: 
-- -- Courses, the menu is the same, but reproduce it!
-- -- 
-- let's say, you have a bunch of individual toggles, you are going through a list and checking things YES or NO
-- I wouldn't use sub-pages for that anyway
-- My sub-pages are TOTALLY separate modules, with little in common

-- Example
-- Course video
-- + Comments
-- + TOC

-- Those are super different. But we don't want it to be a completely separate page, do I?

-- An overlay?

-- No, the ELm way is to have one page per route. I already support that. The ENTIRE Url has to match, nothing more

-- So, modules = pages. They could know their own route, for sure
-- So maybe we should do it with a typeclass instead of a route type
-- Seems possible
-- When you want to address another module... no you'd need a separate router.
-- Link: Course 1 -> All Courses
-- Link: All Courses -> Course 1
-- So they can't call the other *module*
-- You need a single one that links everythin together

-- So, all we need is: Model -> Route
-- but it can't be defined in there
-- Oh, I can define it here!


-- class ToRoute m where
--   toRoute :: m -> Route
-- instance ToRoute Counter.Model where
--   toRoute (Counter.Model c _) = Counter c


-- ROUTES AND PAGES ARE ONE ---------------------
-- what if the Route had the functions on it? The same ADT?
-- perhaps there's a type: (Counter.Params Int), it's what the load method takes. so the Page is 

-- load is known to take the params type

-- data CounterParams = CounterParams Integer

-- -- wait that's not a type
-- -- this has everything we need to render it
-- data CurrentPage
--     = CounterPage Integer (Page Integer Counter.Model Counter.Action)
--     | OtherPage String




-- resolve :: Text -> Maybe (ByteString -> IO Response)
-- resolve t = do
--   r <- fromPath =<< parsePath t
--   pure $ resolveRoute r

-- we need to go from 
-- once we have a page we can run things
-- this boilerplate isn't TERRIBLE
-- but I feel like i'm missing something here...
-- it should be more automatic
-- with fancy typelcasses

-- well, wait, if pages are UNIQUE then we can use a typeclass to resolve routes instead
-- erm. what's the a?

-- URL -> parses to What? A Path. Ok, cool. Whatever.
-- we could have a custom type a-la servant
-- combinated type
-- type App = Counter.Page :<|> Other.Page
-- sweet app bro!
-- then maybe we could nest it arbitarily


-- class Page a where


-- runPage :: CurrentPage -> (ByteString -> IO Response)
-- runPage (CounterPage n page) = runPageAction page n



-- -- This gets the new model, doesn't update the route!
-- resolveRoute :: Route -> (ByteString -> IO Response)
-- resolveRoute (Counter n) = runAction (Counter.load n) Counter.update Counter.view




data Route
    = Counter Integer
    -- | Course CourseId CoursePage
    -- | Courses CourseId CourseSearch
    deriving (Show, Eq)


-- data CoursePage = Contents | Comments
--   deriving (Show, Eq)

-- data CourseSearch = CourseSearch
--   { term :: Text
--   , category :: Text
--   } deriving (Show, Eq)

-- type CourseId = Text


instance RoutePath Route where
  toPath (Counter c) = "counter" :> Num c
  -- toPath (Course i p) = "course" :> Str i :> toPath p
  -- toPath (Courses i s) = "courses" :> Str i :> toPath s

  fromPath ("counter" :> Num n) = Just (Counter n)
  -- fromPath ("course" :> Str i :> cp) = Course i <$> (fromPath cp)
  -- fromPath ("courses" :> Str i :> s) = Courses i <$> (fromPath s)
  fromPath _ = Nothing

-- instance RoutePath CoursePage where
--   toPath Comments = "comments"
--   toPath Contents = "contents"

--   fromPath "comments" = Just Comments
--   fromPath "contents" = Just Contents
--   fromPath _ = Nothing

-- instance RoutePath CourseSearch where
--   toPath (CourseSearch t c) = fields ["term" .: Str t, "category" .: Str c]

--   fromPath (Fields m) = do
--     Str t <- field "term" m
--     Str c <- field "category" m
--     pure $ CourseSearch t c
--   fromPath _ = Nothing
