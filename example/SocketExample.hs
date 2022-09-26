{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module SocketExample where

import Juniper.Prelude
import Juniper.Encode as Encode (Encoded(..), Encoding(..))
import Juniper.Runtime as Runtime (Response(..), runPage, Command, PageHandler)
import Juniper hiding (page)
import Control.Concurrent.STM (newTVar, atomically, TVar)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (MonadBase, liftBase)
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
import Web.Scotty (scotty, ScottyM)
import Web.Scotty.Trans as Scotty (addHeader, file, get, middleware, param, ActionT, ScottyT)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions, ConnectionOptions(..))
import Network.Wai as Wai
import Network.HTTP.Types (status200, status404)
import Sockets
import Text.Read (readMaybe)

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
  routePage _ = Nothing


startWebServer :: TVar [Todo] -> IO ()
startWebServer todos = do

  let cfg = Render False toDocument

  scotty 3031 $ do

    -- we can serve this up static
    -- juniper.js
    get "/juniper.js" $ do
      addHeader "Content-Type" "text/javascript"
      file "dist/main.js"

    get "/example.css" $ do
      addHeader "Content-Type" "text/css"
      file "example/example.css"

    middleware $ juniper cfg run


  where
    run :: (MonadFail m, MonadIO m) => PageHandler AppPage m
    run Focus       = Runtime.runPage Focus.page
    run (Counter n) = Runtime.runPage (Counter.page n)
    run Todos       = Runtime.runPage (Todo.page todos)

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
juniper :: forall page m. (MonadBase m IO, MonadIO m, MonadBaseControl IO m, RoutePage page) => Render -> PageHandler page m -> Middleware
juniper cfg run = web . sockets
  where
    sockets :: Middleware
    sockets = websocketsOr defaultConnectionOptions (liveApp run)

    web :: Middleware
    web app req resp =
      let mp = routePage (pathInfo req) :: Maybe page
      in case mp of
        Nothing -> app req resp
        (Just p) -> do
          res <- liftBase $ run p Nothing []
          Web.respond cfg p res resp :: IO ResponseReceived




-- Do I embed them?
toDocument :: Html () -> Html ()
toDocument = simpleDocument "Example" $ do

  -- add stylesheets, etc
  link_ [type_ "text/css", rel_ "stylesheet", href_ "/example.css"]

  -- Custom Javascript should be last
  script_ [type_ "text/javascript", src_ "/juniper.js"] ("" :: Text)





