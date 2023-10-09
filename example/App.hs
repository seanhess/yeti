{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module App where

import Control.Concurrent.STM (newTVar, atomically, TVar)
import Control.Monad.IO.Class (MonadIO)
-- import Data.Text (Text)
import Page.Route (AppPage(..), mainView)
import Page.Todo (Todo(..))
import Prelude
import Web.Scotty
import Yeti
import qualified Yeti.UI as UI
import qualified Page.Article as Article
import qualified Page.Counter as Counter
import qualified Page.Focus as Focus
import qualified Page.Signup as Signup
import qualified Page.Todo as Todo

start :: IO ()
start = do
  todos <- atomically $ newTVar [Todo "Test Item" Todo.Errand False]
  startServer todos


startServer :: TVar [Todo] -> IO ()
startServer todos = do

  scotty 3031 $ do

    -- Example serves it statically for ease of development. Recommended to embed
    get "/yeti.js" $ do
      addHeader "Content-Type" "text/javascript"
      file "dist/main.js"

    -- get "/ui.css" $ do
    --   addHeader "Content-Type" "text/css"
    --   raw $ UI.stylesheet

    get "/" $ do
      html $ toHtmlLazyText $ document "Example" (pure ()) mainView

    middleware $ yeti config go


  where
    go :: MonadIO m => PageHandler AppPage m
    go Focus       = run Focus.page
    go (Counter n) = run (Counter.page n)
    go Todos       = run (Todo.page todos)
    go Signup      = run Signup.page
    -- go Index       = run mainPage
    go (Article i) = run (Article.page i)



config :: View Content () -> View Document ()
config content =
  document "Example" (pure ()) $ do
    content

    -- yeti source needs to be last
    script (Url "/yeti.js")
          

