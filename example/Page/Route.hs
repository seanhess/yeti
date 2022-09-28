module Page.Route where

import Prelude
import Yeti
import Data.Text (unpack)
import Text.Read (readMaybe)
import Lucid.Html5
import qualified Page.Article as Article

data AppPage
  = Counter Integer
  | Focus
  | Todos
  | Signup
  | Index
  | Article Article.Id
  deriving (Generic, Show, FromJSON, ToJSON)

instance RoutePage AppPage where
  routePage ["counter", n] = do
    cnt <- readMaybe (unpack n)
    pure $ Counter cnt
  routePage ["focus"] = pure Focus
  routePage ["todos"] = pure Todos
  routePage ["signup"] = pure Signup
  routePage ["article", id'] = do
    pure $ Article id'
  routePage ["index"] = pure Index
  routePage _ = Nothing


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