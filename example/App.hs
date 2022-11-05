{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module App where

-- import Control.Concurrent.STM (newTVar, atomically, TVar)
import Control.Monad.IO.Class (MonadIO)
-- import Data.Text (Text)
import Page.Route (AppPage(..), mainView)
-- import Page.Todo (Todo(..))
import Prelude
import Web.Scotty
import Yeti
import Yeti.View.UI
-- import qualified Page.Article as Article
import qualified Page.Counter as Counter
-- import qualified Page.Focus as Focus
-- import qualified Page.Signup as Signup
-- import qualified Page.Todo as Todo

start :: IO ()
start = do
  -- todos <- atomically $ newTVar [Todo "Test Item" Todo.Errand False]
  -- startServer todos
  startServer


-- startServer :: TVar [Todo] -> IO ()
-- startServer todos = do

startServer :: IO ()
startServer = do

  scotty 3031 $ do

    -- we can serve this up static
    -- yeti.js
    get "/yeti.js" $ do
      addHeader "Content-Type" "text/javascript"
      file "dist/main.js"

    get "/app.css" $ do
      addHeader "Content-Type" "text/css"
      file "example/app.css"

    get "/" $ do
      html $ toHtmlLazyText $ document "Example" (pure ()) mainView

    middleware $ yeti config go


  where
    go :: (MonadFail m, MonadIO m) => PageHandler AppPage m
    -- go Focus       = run Focus.page
    go (Counter n) = run (Counter.page n)
    -- go Todos       = run (Todo.page todos)
    -- go Signup      = run Signup.page
    -- go Index       = run mainPage
    -- go (Article i) = run (Article.page i)



-- render expects: content -> document
config :: Render
config = document "Example" $ do
  script' "/yeti.js"
          

