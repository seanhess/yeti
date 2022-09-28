{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import Prelude
import Control.Concurrent.STM (newTVar, atomically)
import Control.Monad.IO.Class (MonadIO)
-- import Debug.Trace (traceM)
import Data.String.Conversions (cs)
import Data.Text as Text (stripPrefix, intercalate, Text)
import Data.Text.IO as Text (readFile)
import qualified Data.Text.Lazy as TL
import Data.Map as Map (fromList, toList, keys)
import Web.Scotty as Scotty
import Network.Wai.Middleware.Static (staticWithOptions, defaultOptions)
import Network.Wai (Request, pathInfo, rawPathInfo, Application)
import Lucid (renderBS, Html)
import Lucid.Html5
-- import Counter (view, Model(..), load, update, view)
-- import App (resolve)
import qualified Page.Counter as Counter
import qualified Page.Signup as Signup
import qualified Page.About as About
import qualified Page.Todo as Todo
import qualified Page.Focus as Focus
import qualified Page.Article as Article
import qualified Page.Comp as Comp
import Page.Todo (Todo(..))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Function ((&))


import Control.Monad.State.Lazy (StateT, execStateT)
import Text.Read (readMaybe)

import Network.HTTP.Types.URI (renderSimpleQuery)

import Yeti.Web (page, lucid, static, handle, simpleDocument, Render(..))




start :: IO ()
start = do

  -- load embedded js
  todos <- atomically $ newTVar [Todo "Test Item" Todo.Errand False]
  let cfg = Render False toDocument

  scotty 3030 $ do
    -- delay to simulate real-world conditions
    -- middleware (delay 500)
    middleware $ staticWithOptions defaultOptions

    page "/app/counter" $ do
      handle cfg Counter.page

    page "/app/signup" $ do
      handle cfg Signup.page

    page "/app/focus" $ do
      handle cfg Focus.page

    page "/app/comp" $ do
      handle cfg Comp.page

    page "/app/todo" $ do
      -- n <- param "n" :: ActionM Int
      -- liftIO $ print n
      handle cfg $ Todo.page todos

    page "/app/article/:id" $ do
      i <- param "id"
      handle cfg $ Article.page i

    -- if you use "lucid" it doesn't work
    get "/app/about" $
      static $ About.view


    get "/hello/:name" $ do
      name <- param "name"
      html $ mconcat ["Hello: ", name]

    page "/test/:message" $ do
      m <- param "message" :: ActionM TL.Text
      html $ "<div id='container'><p>"<> m <>"</p><input type='text'/><p>Hello!</p><button data-click='Action 3'>PRESS</button></div>"

    -- You probably want Render True toDocument to embed the javascript directly
    -- we load dynamically for debugging while developing
    get "/build.js" $ do
      addHeader "Content-Type" "text/javascript"
      file "edom/build.js"

    get "/run.js" $ do
      addHeader "Content-Type" "text/javascript"
      file "edom/run.js"

    get "/" $ do
      html $ cs $ renderBS $ ol_ [] $ do
        li_ $ a_ [href_ "/app/counter"] "Counter"
        li_ $ a_ [href_ "/app/signup"] "Signup"
        li_ $ a_ [href_ "/app/focus"] "Focus"
        li_ $ a_ [href_ "/app/todo"] "Todo"
        li_ $ a_ [href_ "/app/comp"] "Comp"
        li_ $ a_ [href_ "/app/article/1"] "Article"


-- Consider creating your own function (Html () -> Html ()) to pass to render
-- that exactly describes your document
toDocument :: Html () -> Html ()
toDocument = simpleDocument "Example" $ do

  -- add stylesheets, etc
  link_ [type_ "text/css", rel_ "stylesheet", href_ "/example/example.css"]

  -- In your application, you probably want to embed this javascript via 
  --  > let cfg = Render True toDocument
  script_ [type_ "text/javascript", src_ "/build.js"] ("" :: Text)
  script_ [type_ "text/javascript", src_ "/run.js"] ("" :: Text)

  -- Custom Javascript should be last
  script_ [type_ "text/javascript", src_ "/example/example.js"] ("" :: Text)


delay :: Int -> Application -> Application
delay d application req respond = do
  threadDelay (d * 1000)
  application req respond



