{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module SocketExample where

import Juniper.Prelude
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
import Web.Scotty.Trans as Scotty (addHeader, file, get)


import Sockets


startLive :: IO ()
startLive = do
  todos <- atomically $ newTVar [Todo "Test Item" Todo.Errand False]
  concurrent
    (startWebServer todos)
    (startSocket todos)






startWebServer :: TVar [Todo] -> IO ()
startWebServer todos = do


  let cfg = Render False toDocument

  scotty 3031 $ do
    get "/live.js" $ do
      addHeader "Content-Type" "text/javascript"
      file "dist/main.js"

    -- get "/" $ do
    --   -- handle cfg Counter.page
    --   handle cfg Focus.page
    --   -- html $
    --   --   "This is a test <script src='/live.js'></script>"

    pageRoute cfg "/focus" Focus.page
    pageRoute cfg "/counter" Counter.page
    pageRoute cfg "/todo" (Todo.page todos)
 
    -- page "focus" Focus
    -- it's (model -> Page') that does the trick




toDocument :: Html () -> Html ()
toDocument = simpleDocument "Example" $ do

  -- add stylesheets, etc
  -- link_ [type_ "text/css", rel_ "stylesheet", href_ "/example/example.css"]

  -- In your application, you probably want to embed this javascript via 
  --  > let cfg = Render True toDocument
  -- script_ [type_ "text/javascript", src_ "/build.js"] ("" :: Text)
  -- script_ [type_ "text/javascript", src_ "/run.js"] ("" :: Text)

  -- Custom Javascript should be last
  script_ [type_ "text/javascript", src_ "/live.js"] ("" :: Text)







data Page'
  = Counter Counter.Model
  | Focus Focus.Model
  | Todos Todo.Model
  deriving (Generic, Show)

instance FromJSON Page' where
  parseJSON v =
        Counter <$> parseJSON v
    <|> Focus <$> parseJSON v
    <|> Todos <$> parseJSON v


-- can you run this in other than IO?
-- not for now
startSocket :: TVar [Todo] -> IO ()
startSocket todos = do
  startLiveView $ \pg ->
    case pg of
      Counter m -> do
        register Counter.page m

      Focus m -> do
        register Focus.page m

      Todos m -> do
        register (Todo.page todos) m

