module Page.Route where

import Prelude
import Data.Text (pack)
import Yeti
import Yeti.View.UI
-- import qualified Page.Article as Article

data AppPage
  = Counter Integer
  -- | Index
  deriving (Generic, Show, RoutePage)

  -- -- = Focus
  -- -- | Todos
  -- -- | Signup
  -- -- | Article Article.Id

mainView :: View Content ()
mainView = do
  row (p S1) $ do
    page (Counter 11)
    -- page Signup
    -- page Focus
    -- page Todos
    -- page (Article "1")
  where
    page :: AppPage -> View Content ()
    page p' = 
      el id $ link (pageUrlPath p') (fromText $ pack $ show p')

-- Example of how to do a completely static page
mainPage :: Applicative m => StaticPage m
mainPage = staticPage mainView