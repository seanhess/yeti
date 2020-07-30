{-# LANGUAGE OverloadedStrings #-}
module App where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Counter
import Wookie.Router
import Wookie.Runtime
import Lucid (Html)







resolve :: Text -> Maybe (ByteString -> IO Response)
resolve t = do
  r <- fromPath =<< parsePath t
  pure $ resolveRoute r


resolveRoute :: Route -> (ByteString -> IO Response)
resolveRoute (Counter n) = runtime (Counter.load n) Counter.update Counter.view



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
