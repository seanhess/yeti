{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module SocketExample where

import Juniper.Prelude
import Juniper.Encode as Encode (Encoded(..), Encoding(..))
import Juniper.Runtime as Runtime (Response(..), runPage, Command, Handler)
import Juniper hiding (page)
import Control.Concurrent.STM (newTVar, atomically, TVar)
import Data.Char (isPunctuation, isSpace)
import qualified Data.Map as Map
import Data.Monoid (mappend)
import Data.Text (Text)
import Lucid
import Juniper.Params as Params
import qualified Juniper.Web as Web
import qualified Page.Counter as Counter
import qualified Page.Focus as Focus
import qualified Page.Todo as Todo
import Page.Todo (Todo(..))
import GHC.Generics
import Data.Aeson (FromJSON(..))
import Web.Scotty (scotty)
import Web.Scotty.Trans as Scotty (addHeader, file, get, middleware)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions, ConnectionOptions(..))
import Network.Wai (Application)
import Sockets

-- TODO Actually wire up the generalized function
-- TODO Params


mainLive :: IO ()
mainLive = do
  todos <- atomically $ newTVar [Todo "Test Item" Todo.Errand False]
  startWebServer todos






data AppPage
  = Counter
  | Focus
  | Todos
  deriving (Generic, Show, FromJSON, ToJSON)


startWebServer :: TVar [Todo] -> IO ()
startWebServer todos = do

  let cfg = Render False toDocument

  scotty 3031 $ do
    middleware socketMiddleware

    -- we can serve this up static
    -- juniper.js
    get "/juniper.js" $ do
      addHeader "Content-Type" "text/javascript"
      file "dist/main.js"

    get "/focus" $ do
      Web.handle cfg Focus run

    get "/counter" $ do
      Web.handle cfg Counter run

    get "/todos" $ do
      Web.handle cfg Todos run

    -- pageRoute cfg "/focus"   Focus.page
    -- pageRoute cfg "/counter" Counter.page
    -- pageRoute cfg "/todo"    (Todo.page todos)

  where


    socketMiddleware :: Application -> Application
    socketMiddleware app = do
      websocketsOr defaultConnectionOptions (application run) app

    run :: (MonadFail m, MonadIO m) => Handler AppPage m
    run Focus   = Runtime.runPage Focus.page
    run Counter = Runtime.runPage Counter.page
    run Todos   = Runtime.runPage (Todo.page todos)



toDocument :: Html () -> Html ()
toDocument = simpleDocument "Example" $ do

  -- add stylesheets, etc
  -- link_ [type_ "text/css", rel_ "stylesheet", href_ "/example/example.css"]

  -- In your application, you probably want to embed this javascript via 
  --  > let cfg = Render True toDocument
  -- script_ [type_ "text/javascript", src_ "/build.js"] ("" :: Text)
  -- script_ [type_ "text/javascript", src_ "/run.js"] ("" :: Text)

  -- Custom Javascript should be last
  script_ [type_ "text/javascript", src_ "/juniper.js"] ("" :: Text)





