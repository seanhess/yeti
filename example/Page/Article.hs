{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Article where

import Prelude
import Yeti
-- import Error (PageError(..))
import Data.Text (Text, unpack)
import Control.Monad (forM_)
import Control.Exception (throw, Exception)
import Yeti.UI
import qualified Data.List as List


data PageError = NotFound String
  deriving (Show, Exception)


data Model = Model
  { article  :: Article
  , comment :: Text
  , comments :: [Text]
  } deriving (Generic, LiveModel)

fakeDatabase :: [(Id, Article)]
fakeDatabase = map (\a -> (a.articleId, a))
  [ Article "1" "Article 1"
  , Article "2" "Article 2"
  , Article "3" "Article 3"
  , Article "4" "Article 4"
  , Article "5" "Article 5"
  ]

type Id = Text
data Article = Article
  { articleId :: Id
  , articleText :: Text
  } deriving (Generic, ToJSON, FromJSON)

data Action
  = Comment Text
  | SubmitComment
  deriving (Show, Generic, LiveAction)

-- TODO this page should be able to throw a 404 and hit the handler if it wants
load :: (Monad m) => Id -> m Model
load i = do
  case List.lookup i fakeDatabase of
    Nothing -> throw $ NotFound $ "Article " <> unpack i
    Just a -> pure $ Model a "" []

update :: Monad m => Action -> Model -> m Model
update (Comment t) m =
  pure $ m { comment = t }
update SubmitComment m =
  pure $ m { comments = m.comments <> [m.comment], comment = "" }

view :: Model -> View Content ()
view m = col (gap 10) $ do
    el bold $ text_ $ "Article: " <> m.article.articleId

    el (pad 10) $ text_ m.article.articleText

    -- BUG:, if there are no contents, it puts the next thing into this parent!!
    col (gap 6) $ do
      forM_ m.comments $ \c -> do
        text_ $ "* " <> c

    col (gap 10) $ do
      field (gap 8) LabelAbove "Comment" $ do
        inputText Comment m.comment (onEnter SubmitComment)

      button SubmitComment id "Submit"

test :: View Content ()
test = do
  col (gap 10) $ do
    col (gap 7) (pure ())
    text_ "xxx"



page :: Monad m => Id -> Page () Model Action m
page i = simplePage (load i) update view
