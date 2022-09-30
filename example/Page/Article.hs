{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Article where

import Prelude
import Yeti
import Error (PageError(..))
import Data.Text (Text, unpack)
import Control.Monad (forM_)
import Control.Exception (throw)
import Lucid (toHtml)
import Lucid.Html5
import qualified Data.List as List


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
  | TestEnter
  | Woot Text
  deriving (Show, Generic, LiveAction)

-- TODO this page should be able to throw a 404 and hit the handler if it wants
load :: (MonadFail m) => Id -> m Model
load i = do
  case List.lookup i fakeDatabase of
    Nothing -> throw $ NotFound $ "Article " <> unpack i
    Just a -> pure $ Model a "" []

update :: Monad m => Action -> Model -> m Model
update (Comment t) m =
  pure $ m { comment = t }
update SubmitComment m =
  pure $ m { comments = m.comments <> [m.comment], comment = "" }
update TestEnter m =
  pure $ m { comments = m.comments <> ["Enter"] }
update (Woot t) m =
  pure $ m { comments = m.comments <> ["Woot"] }

view :: Model -> Html ()
view m = section_ [ class_ "page" ] $ do
    h1_ $ toHtml $ "Article: " <> m.article.articleId

    div_ [ class_ "section" ] $ do
      p_ $ toHtml m.article.articleText

    ol_ [ class_ "section"] $ do
      forM_ m.comments $ \c -> do
        div_ $ toHtml c

    div_ [ class_ "section" ] $ do
      input' [ value_ m.comment, onInput Comment, onEnter SubmitComment ] ""
      button_ [ onClick SubmitComment ] "Submit"


page :: MonadFail m => Id -> Page () Model Action m
page i = simplePage (load i) update view
