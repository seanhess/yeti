{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module App where

import Control.Concurrent.STM (newTVar, atomically, TVar)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Lucid
import Page.Route (AppPage(..), mainPage, mainView)
import Page.Todo (Todo(..))
import Prelude
import Web.Scotty
import Yeti
import qualified Page.Article as Article
import qualified Page.Counter as Counter
import qualified Page.Focus as Focus
import qualified Page.Signup as Signup
import qualified Page.Todo as Todo

-- TODO make root page be an actual static page. Add "staticPage" to wherver page is defined to make it easy?

start :: IO ()
start = do
  todos <- atomically $ newTVar [Todo "Test Item" Todo.Errand False]
  startServer todos


startServer :: TVar [Todo] -> IO ()
startServer todos = do

  scotty 3031 $ do

    -- we can serve this up static
    -- yeti.js
    get "/yeti.js" $ do
      addHeader "Content-Type" "text/javascript"
      file "dist/main.js"

    get "/example.css" $ do
      addHeader "Content-Type" "text/css"
      file "example/example.css"

    get "/" $ do
      html $ Lucid.renderText $ mainView

    middleware $ yeti config go


  where
    go :: (MonadFail m, MonadIO m) => PageHandler AppPage m
    go Focus       = run Focus.page
    go (Counter n) = run (Counter.page n)
    go Todos       = run (Todo.page todos)
    go Signup      = run Signup.page
    go Index       = run mainPage
    go (Article i) = run (Article.page i)


    config :: Render
    config = Render False $ simpleDocument "Example" $ do

      -- add stylesheets, etc
      link_ [type_ "text/css", rel_ "stylesheet", href_ "/example.css"]

      -- Custom Javascript should be last
      script_ [type_ "text/javascript", src_ "/yeti.js"] ("" :: Text)


