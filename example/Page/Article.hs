{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Article where

import Prelude
import Juniper
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_)
import Lucid (Html, toHtml)
import Lucid.Html5
import qualified Data.List as List


data Model = Model
  { article  :: Article
  , comment :: Text
  , comments :: [Text]
  } deriving (Show, Read, Encode LiveModel)

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
  } deriving (Read, Show)


data Action
  = Comment Value
  | SubmitComment
  deriving (Show, Read, Encode LiveAction)

-- TODO this should be a 404 if the post is missing!
-- could I return a maybe model instead? A nothing?
-- there could be a list of cool things
-- Model myModel
-- Missing message
-- etc...
load :: (MonadFail m) => Id -> m Model
load i = do
  case List.lookup i fakeDatabase of
    Nothing -> fail "Could not find article!"
    Just a -> pure $ Model a "" []

update :: Monad m => Action -> Model -> m Model
update (Comment (Value t)) m =
  pure $ m { comment = t }
update SubmitComment m =
  pure $ m { comments = m.comments <> [m.comment], comment = "" }

view :: Model -> Html ()
view m = section_ [ class_ "page" ] $ do
    h1_ $ toHtml $ "Article: " <> m.article.articleId

    div_ [ class_ "section" ] $ do
      p_ $ toHtml m.article.articleText

    ol_ [ class_ "section"] $ do
      forM_ m.comments $ \c -> do
        div_ $ toHtml c

    div_ [ class_ "section" ] $ do
      input_ [ value_ m.comment, onInput Comment, onEnter SubmitComment ]
      button_ [ onClick SubmitComment ] "Submit"


page :: MonadFail m => Id -> Page () Model Action m
page i = simplePage (load i) update view
