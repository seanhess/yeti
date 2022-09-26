{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module SocketExample where

import Juniper.Prelude
import Juniper.Encode as Encode (Encoded(..), Encoding(..))
import Juniper.Runtime as Runtime (Response(..), runPage, Command)
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


mainLive :: IO ()
mainLive = do
  todos <- atomically $ newTVar [Todo "Test Item" Todo.Errand False]
  pure ()
  -- concurrent
  --   (startWebServer todos)
    -- (startSocket todos)


startLive :: TVar [Todo] -> IO ()
startLive todos = 
  pure ()
  where
    run :: (MonadFail m, MonadIO m) => AppPage -> Encoded 'Encode.Model -> [Encoded 'Encode.Action] -> m Response
    run Focus m as   = Runtime.runPage Focus.page m as
    run Counter m as = Runtime.runPage Counter.page m as
    run Todos m as   = Runtime.runPage (Todo.page todos) m as



data AppPage
  = Counter
  | Focus
  | Todos
  deriving (Generic, Show)




-- -- can you run this in other than IO?
-- -- not for now
-- startSocket :: TVar [Todo] -> IO ()
-- startSocket todos = do
--   startLiveView $ \pg ->
--     case pg of
--       Counter -> do
--         register Counter.page m

--       Focus -> do
--         register Focus.page m

--       Todos -> do
--         register (Todo.page todos) m





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

    pageRoute cfg "/focus"   Focus.page
    pageRoute cfg "/counter" Counter.page
    pageRoute cfg "/todo"    (Todo.page todos)
 
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





