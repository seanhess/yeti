{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoOverloadedLists #-}
module SocketExample where

import Juniper.Prelude
import Juniper hiding (page)
import Data.Char (isPunctuation, isSpace)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Monad.Loops (iterateM_)
import Lucid
import Juniper.Encode
import qualified Juniper.Runtime as Runtime hiding (run)
import Control.Concurrent (forkIO)
import Control.Exception (finally, Exception, throw)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, putMVar)
import qualified Page.Counter as Counter
import qualified Page.Focus as Focus
import GHC.Generics
import Data.Aeson as Aeson (ToJSON, FromJSON(..), decode, Result(..))
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.UUID as UUID (UUID)
import Data.UUID.V4 as UUID

import qualified Network.WebSockets as WS
import Sockets


import Web.Scotty as Scotty


start :: IO ()
start = do
  forkIO startSocket
  startWebServer
  pure ()


startWebServer :: IO ()
startWebServer = do

  let cfg = Render False toDocument

  scotty 3031 $ do
    get "/live.js" $ do
      addHeader "Content-Type" "text/javascript"
      file "edom/live.js"

    get "/" $ do
      handle cfg Counter.page
      -- html $
      --   "This is a test <script src='/live.js'></script>"


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






newtype Message = Message { fromMessage :: ByteString }
  deriving newtype (Eq, WS.WebSocketsData)

instance Show Message where
  show (Message m) = " |> " <> cs m



-- List of Functions that MIGHT parse it
-- Register them

-- page (parse :: CounterCounter..Model)
-- no, they have to be the same type

data Page'
  = Counter Counter.Model
  | Focus Focus.Model
  deriving (Generic, Show)

instance FromJSON Page' where
  parseJSON v =
        Counter <$> parseJSON v
    <|> Focus <$> parseJSON v


-- can you run this in other than IO?
-- not for now
startSocket :: IO ()
startSocket = do
  startLiveView liftIO $ \pg ->
    case pg of
      Counter m -> do
        runRegister Counter.page m
      Focus m -> do
        runRegister Focus.page m



-- we could just have all of this run in their monad?

startLiveView :: forall page m a. (MonadIO m, FromJSON page, Show page) => (m (Html ()) -> IO (Html ())) -> (page -> IO (Message -> m (Html ()))) -> IO ()
startLiveView toIO pageRun = do

  liftIO $ WS.runServer "127.0.0.1" 9160 $ \pending -> do
    conn <- WS.acceptRequest pending
    page <- identify conn
    run <- pageRun page
    connect toIO run conn

  where
    identify :: WS.Connection -> IO page
    identify conn = do
      msg <- WS.receiveData conn :: IO Message
      case Aeson.decode (fromMessage msg) of
        Nothing -> do
          throw $ NoIdentify msg
        Just p -> do
          putStrLn "Identified"
          print p
          WS.sendTextData conn ("Identified" :: Text)
          pure p




connect :: forall m x. (m (Html ()) -> IO (Html ())) -> (Message -> m (Html ())) -> WS.Connection -> IO ()
connect toIO run conn = do
  putStrLn "Connect"
  WS.withPingThread conn 30 (return ()) $ do
    finally disconnect $ forever talk

  where

    talk :: IO ()
    talk = do
      -- wait for them to send actions
      putStrLn "Talk"
      msg <- WS.receiveData conn :: IO Message
      print msg

      h <- toIO $ run msg
      let bs = Lucid.renderBS h :: ByteString
      putStrLn $ cs bs
      WS.sendTextData conn bs

    disconnect :: IO ()
    disconnect = do
      -- perform any cleanup here
      pure ()







runRegister :: (LiveAction action, MonadIO m, Show model) => Page params model action m -> model -> m (Message -> m (Html ()))
runRegister pg m = do
  putStrLn "Register"
  st <- liftIO $ newMVar m
  putStrLn "Registered"
  pure $ runUpdateRender pg st

runUpdateRender :: (LiveAction action, MonadIO m, Show model) => Page params model action m -> MVar model -> Message -> m (Html ())
runUpdateRender pg st msg = do
  putStrLn "runUpdateRender"
  m <- liftIO $ readMVar st
  putStrLn "got M"
  -- what if this throws an exception? Doesn't matter, All I did was read it
  m' <- update pg msg m
  print m'
  liftIO $ putMVar st m
  putStrLn "Saved"
  pure $ (Runtime.view pg) m

update :: (LiveAction action, Monad m) => Page params model action m -> Message -> model -> m model
update pg msg m = do
  case decodeAction (cs $ fromMessage msg) of
    Error _ -> throw $ InvalidAction msg
    Success act -> do
      m' <- (Runtime.update pg) act m
      pure m'





data SocketError
  = NoIdentify Message
  | InvalidAction Message
  deriving (Show, Exception)


-- ok, that's easy





