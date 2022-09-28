{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module SocketExample where

import Control.Concurrent.STM (newTVar, atomically, TVar)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (MonadBase, liftBase)
import Data.Char (isPunctuation, isSpace)
import qualified Data.Map as Map
import Data.Monoid (mappend)
import Data.Text (Text)
import Lucid
import Yeti.Params as Params
import qualified Yeti.Web as Web
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
import Sockets
import Text.Read (readMaybe)
import Yeti.Prelude
import Yeti.Encode as Encode (Encoded(..), Encoding(..))
import Yeti.Runtime as Runtime (Response(..), runPage, Command, PageHandler)
import Yeti hiding (page)

-- TODO Actually wire up the generalized function
-- TODO Params


mainLive :: IO ()
mainLive = do
  todos <- atomically $ newTVar [Todo "Test Item" Todo.Errand False]
  startWebServer todos






data AppPage
  = Counter Integer
  | Focus
  | Todos
  | Signup
  | Article Article.Id
  deriving (Generic, Show, FromJSON, ToJSON)

-- TODO better encoding
class (Show page, ToJSON page, FromJSON page) => RoutePage page where
  routePage :: [Text] -> Maybe page

instance RoutePage AppPage where
  routePage ["counter", n] = do
    cnt <- readMaybe (cs n)
    pure $ Counter cnt
  routePage ["focus"] = pure Focus
  routePage ["todos"] = pure Todos
  routePage ["signup"] = pure Signup
  routePage ["article", id] = do
    pure $ Article (cs id)
  routePage _ = Nothing


startWebServer :: TVar [Todo] -> IO ()
startWebServer todos = do

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
      html $ cs $ renderBS $ ol_ [] $ do
        li_ $ a_ [href_ "/counter/11"] "Counter 11"
        li_ $ a_ [href_ "/signup"] "Signup"
        li_ $ a_ [href_ "/focus"] "Focus"
        li_ $ a_ [href_ "/todo"] "Todo"
        -- li_ $ a_ [href_ "/app/comp"] "Comp"
        li_ $ a_ [href_ "/article/1"] "Article"

    middleware $ yeti config run


  where
    run :: (MonadFail m, MonadIO m) => PageHandler AppPage m
    run Focus       = Runtime.runPage Focus.page
    run (Counter n) = Runtime.runPage (Counter.page n)
    run Todos       = Runtime.runPage (Todo.page todos)
    run Signup      = Runtime.runPage Signup.page
    run (Article i) = Runtime.runPage (Article.page i)


    config :: Render
    config = Render False $ simpleDocument "Example" $ do

      -- add stylesheets, etc
      link_ [type_ "text/css", rel_ "stylesheet", href_ "/example.css"]

      -- Custom Javascript should be last
      script_ [type_ "text/javascript", src_ "/yeti.js"] ("" :: Text)





-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
yeti :: forall page m. (MonadBase m IO, MonadIO m, MonadBaseControl IO m, RoutePage page) => Render -> PageHandler page m -> Middleware
yeti cfg run = web . sockets
  where
    sockets :: Middleware
    sockets = websocketsOr defaultConnectionOptions (liveApp run)

    web :: Middleware
    web app req resp =
      let mp = routePage (Wai.pathInfo req) :: Maybe page
          qt = queryToQueryText (Wai.queryString req)
      in case mp of
        Nothing -> app req resp
        (Just p) -> do
          res <- liftBase $ run p Nothing qt []
          Web.respond cfg p res resp :: IO ResponseReceived



