module Page.Route where

import Prelude
import Yeti
import Lucid.Html5
import qualified Page.Article as Article

data AppPage
  = Focus
  | Counter Integer
  | Todos
  | Signup
  | Index
  | Article Article.Id
  deriving (Generic, Show, FromJSON, ToJSON, RoutePage)


mainView :: Html ()
mainView = do
  ol_ [] $ do
    li_ $ a_ [href_ "/counter/11"] "Counter 11"
    li_ $ a_ [href_ "/signup"] "Signup"
    li_ $ a_ [href_ "/focus"] "Focus"
    li_ $ a_ [href_ "/todos"] "Todo"
    li_ $ a_ [href_ "/article/1"] "Article"

-- Example of how to do a completely static page
mainPage :: Applicative m => StaticPage m
mainPage = staticPage mainView