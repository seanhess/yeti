module Sockets where

import Juniper.Prelude
import Lucid

import qualified Data.Text as Text
import qualified Juniper.Runtime as Runtime
import Juniper.Runtime (Response(..))
import Juniper hiding (page)
import Juniper.Encode (Encoded(..), Encoding(..))
import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets as WS
import Network.WebSockets (WebSocketsData)
import Control.Exception.Lifted (finally, Exception, throw, mask, onException, catch, AsyncException)
import Control.Concurrent (forkIO, ThreadId, throwTo)
import Control.Concurrent.MVar.Lifted (MVar, newMVar, modifyMVar)
import Control.Monad (forever, forM_)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (MonadBase, liftBase)
import Data.Aeson as Aeson
import Data.Aeson.Types
import Web.Scotty.Trans as Scotty
-- import Network.WebSockets.Trans (liftServerApp, runServerAppT, ServerAppT)
import Network.WebSockets.Connection (defaultConnectionOptions, ConnectionOptions(..))

-- class SPage model m | model -> m where
--   type Msg model :: *
--   type Params model :: *

--   -- this has to work for ALL m
--   load' :: m model
--   params' :: model -> Params model
--   update' :: Msg model -> model -> m model
--   view' :: model -> Html ()

-- TODO can we do this with just the constructor?
pageRoute :: (MonadIO m, LiveModel mod, LiveAction act, ToParams prm, ScottyError e) => Render -> RoutePattern -> Page prm mod act (ActionT e m) -> ScottyT e m ()
pageRoute cfg r pg = do
  -- 1. load, then go
  Scotty.get r $ do
    handle cfg pg

    pure ()




newtype Message = Message { fromMessage :: Text }
  deriving newtype (Eq, Show, WebSocketsData)


-- instance Show Message where
--   show (Message m) = "|" <> cs m <> "|"



-- we could just have all of this run in their monad?

-- run :: (MonadFail m, MonadIO m) => AppPage -> Encoded 'Encode.Model -> [Encoded 'Encode.Action] -> m Response
-- (page -> IO (WS.Connection -> IO ()))
startLiveView
  :: forall page m a. (MonadBase m IO, MonadIO m, MonadBaseControl IO m, FromJSON page, Show page)
  => (page -> Encoded 'Model -> [Encoded 'Action] -> m Response)
  -> IO ()
startLiveView pageResponse = do
  putStrLn "startLiveView"
  WS.runServer "127.0.0.1" 9160 (application pageResponse)


application
  :: forall page m a. (MonadBase m IO, MonadIO m, MonadBaseControl IO m, FromJSON page, Show page)
  => (page -> Encoded 'Model -> [Encoded 'Action] -> m Response)
  -> WS.PendingConnection
  -> IO ()
application pageResponse pending = do
  putStrLn "Connection!"

  flip catch onError $ do
    conn <- liftIO $ WS.acceptRequest pending
    id <- identify conn
    print (page id)
    run <- pageRun id
    connect run conn

  where

    pageRun :: Identified page -> IO (WS.Connection -> IO ())
    pageRun i = do
      var <- newMVar (state i)
      pure $ \conn -> liftBase $ talk i var pageResponse conn

    -- this needs to return page and state
    identify :: WS.Connection -> IO (Identified page)
    identify conn = do
      putStrLn "Waiting for Identify"
      msg <- WS.receiveData conn
      print msg
      id <- parseIdentify $ Text.splitOn "\n" $ fromMessage msg
      putStrLn "Identified"
      pure id

    -- Format:
    -- <Page>
    -- <Encoded Model>
    parseIdentify :: [Text] -> IO (Identified page)
    parseIdentify [pt, mt] = do
      p <- parsePage pt
      pure $ Identified p (Encoded mt)
    parseIdentify t = do
      throw $ NoIdentify t

    parsePage :: Text -> IO page
    parsePage p =
      case Aeson.decode (cs p) of
        Nothing -> do
          throw $ NoIdentifyPage p
        Just pg -> do
          pure pg

    connect :: (WS.Connection -> IO ()) -> WS.Connection -> IO ()
    connect run conn = do
      putStrLn "CONNECT"

      WS.withPingThread conn 30 (return ()) $ do
        -- finally disconnect $ forever (run conn)
        forever (run conn)


    onError :: SocketError -> IO ()
    onError e = do
      putStrLn $ "ERROR!"
      print e

    disconnect :: IO ()
    disconnect = do
      -- perform any cleanup here
      putStrLn "DISCONNECT!"
      pure ()


data Identified page = Identified
  { page :: page
  , state :: Encoded 'Model
  }

-- It's all right here, except for the connection
-- run :: (MonadFail m, MonadIO m) => AppPage -> Encoded 'Encode.Model -> [Encoded 'Encode.Action] -> m Response
talk
  :: forall page m. (Show page, MonadIO m, MonadBase IO m, MonadBase m IO, MonadBaseControl IO m)
  => Identified page
  -> MVar (Encoded 'Model)
  -> (page -> Encoded 'Model -> [Encoded 'Action] -> m Response)
  -> WS.Connection ->
  m ()
talk (Identified page encModel) state run conn = do
  putStrLn $ "TALK: " <> (show page)
  msg <- liftIO $ WS.receiveData conn :: m Message

  res <- updateState state msg

  -- FORMAT
  -- state
  -- html
  liftIO $ WS.sendTextData conn $ mconcat
    [ cs $ fromEncoded $ resModel res
    , "\n"
    , Lucid.renderBS (resView res)
    ]

  where



    updateState :: MVar (Encoded 'Model) -> Message -> m Response
    updateState st msg = do
      res <- modifyMVar st updateEnc
      pure res

      where
        -- no type safety yet
        -- grr 
        updateEnc :: Encoded 'Model -> m (Encoded 'Model, Response)
        updateEnc em = do
          -- (Runtime.update page)
          let als = Text.splitOn "\n" $ fromMessage msg :: [Text]
          let encActions = map Encoded als :: [Encoded 'Action]
          res <- run page em encActions
          pure (resModel res, res)


        -- TODO move to Runtime
        parseModel :: LiveModel model => Encoded 'Model -> m model
        parseModel em = do
          case decodeModel encModel of
            Error e -> throw $ InvalidModel (show page) em
            Success m -> pure m


data SocketError
  = NoIdentify [Text]
  | NoIdentifyPage Text
  | InvalidModel String (Encoded 'Model)
  | InvalidAction String Message
  deriving (Show, Exception)





-- Taken from Control.Concurrent. Returns the modified variable at the end
-- modifyMVar :: MVar a -> (a -> IO a) -> IO a
-- modifyMVar m io =
--   mask $ \restore -> do
--     a  <- takeMVar m
--     a' <- restore (io a) `onException` putMVar m a
--     putMVar m a'
--     pure a'

-- Run two actions, forward exceptions to the forked thread
concurrent :: IO () -> IO () -> IO ()
concurrent act1 act2 = do
  t <- (forkIO act1)
  act2 `catch` (onInterrupt t)
  where
    onInterrupt :: ThreadId -> AsyncException -> IO ()
    onInterrupt t e = do
      throwTo t e
      throw e