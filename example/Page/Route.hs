module Page.Route where

import Prelude
import Yeti
import Lucid.Html5
import Lucid (toHtml)
import qualified Page.Article as Article

data AppPage
  = Focus
  | Counter Integer
  | Todos
  | Signup
  | Index
  | Article Article.Id
  deriving (Generic, Show, RoutePage)


mainView :: Html ()
mainView = do
  ol_ [] $ do
    page (Counter 11)
    page Signup
    page Focus
    page Todos
    page (Article "1")
  where
    page :: AppPage -> Html ()
    page p = 
      li_ $ a_ [href_ $ pageUrlPath p] (toHtml $ show p)

-- Example of how to do a completely static page
mainPage :: Applicative m => StaticPage m
mainPage = staticPage mainView