{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module SocketExample where

import Prelude
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent.STM (newTVar, atomically, TVar)
import Data.Char (isPunctuation, isSpace)
import qualified Data.Map as Map
import Data.Monoid (mappend)
import Data.Text (Text, unpack)
import Lucid
import Yeti.Params as Params
import qualified Yeti.Server as Server
import qualified Page.Counter as Counter
import qualified Page.Focus as Focus
import qualified Page.Todo as Todo
import qualified Page.Article as Article
import qualified Page.Signup as Signup
import qualified Page.Todo as Todo
import Page.Todo (Todo(..))
import GHC.Generics
import Data.Aeson (FromJSON(..))
import Web.Scotty (scotty, ScottyM)
import Web.Scotty.Trans as Scotty (addHeader, file, get, middleware, param, ActionT, ScottyT, html)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions, ConnectionOptions(..))
import Network.Wai as Wai
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.URI (queryToQueryText)
import Text.Read (readMaybe)
import Yeti

-- TODO Actually wire up the generalized function
-- TODO Params


mainLive :: IO ()
mainLive = do
  todos <- atomically $ newTVar [Todo "Test Item" Todo.Errand False]
  startServer todos






data AppPage
  = Counter Integer
  | Focus
  | Todos
  | Signup
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
  routePage _ = Nothing

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
      html $ Lucid.renderText $ ol_ [] $ do
        li_ $ a_ [href_ "/counter/11"] "Counter 11"
        li_ $ a_ [href_ "/signup"] "Signup"
        li_ $ a_ [href_ "/focus"] "Focus"
        li_ $ a_ [href_ "/todo"] "Todo"
        -- li_ $ a_ [href_ "/app/comp"] "Comp"
        li_ $ a_ [href_ "/article/1"] "Article"

    middleware $ yeti config go


  where
    go :: (MonadFail m, MonadIO m) => PageHandler AppPage m
    go Focus       = run Focus.page
    go (Counter n) = run (Counter.page n)
    go Todos       = run (Todo.page todos)
    go Signup      = run Signup.page
    go (Article i) = run (Article.page i)


    config :: Render
    config = Render False $ simpleDocument "Example" $ do

      -- add stylesheets, etc
      link_ [type_ "text/css", rel_ "stylesheet", href_ "/example.css"]

      -- Custom Javascript should be last
      script_ [type_ "text/javascript", src_ "/yeti.js"] ("" :: Text)


