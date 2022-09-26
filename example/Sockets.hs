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
import Control.Monad.Base (MonadBase)
import Data.Aeson as Aeson
import Data.Aeson.Types
import Web.Scotty.Trans as Scotty
import Network.WebSockets.Trans (liftServerApp, runServerAppT, ServerAppT)

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




newtype Message = Message { fromMessage :: [Text] }
  deriving newtype (Eq, Show)

instance WebSocketsData Message where


-- instance Show Message where
--   show (Message m) = "|" <> cs m <> "|"



-- we could just have all of this run in their monad?

-- run :: (MonadFail m, MonadIO m) => AppPage -> Encoded 'Encode.Model -> [Encoded 'Encode.Action] -> m Response
-- (page -> IO (WS.Connection -> IO ()))
startLiveView :: forall page m a. (MonadBase IO m, MonadBaseControl IO m, MonadIO m) => (FromJSON page, Show page) => (page -> Encoded 'Model -> [Encoded 'Action] -> m Response) -> m ()
startLiveView pageResponse = do

  putStrLn "startLiveView"
  app <- runServerAppT accept :: m WS.ServerApp
  liftIO $ WS.runServer "127.0.0.1" 9160 app

  where

    accept :: WS.PendingConnection -> m ()
    accept pending = do
      putStrLn "Connection!"

      conn <- liftIO $ WS.acceptRequest pending
      id <- identify conn
      var <- newMVar (state id)

      let run = pageRun id var
      connect run conn `catch` onError

    pageRun :: Identified page -> MVar (Encoded 'Model) -> WS.Connection -> m ()
    pageRun i var =
      register i pageResponse var

    -- this needs to return page and state
    identify :: WS.Connection -> m (Identified page)
    identify conn = do
      msg <- liftIO $ WS.receiveData conn :: m Message
      id <- parseIdentify msg
      putStrLn "Identified"
      pure id

    -- Format:
    -- <Page>
    -- <Encoded Model>
    parseIdentify :: Message -> m (Identified page)
    parseIdentify (Message [pt, mt]) = do
      p <- parsePage pt
      pure $ Identified p (Encoded mt)

    parsePage :: Text -> m page
    parsePage p =
      case Aeson.decode (cs p) of
        Nothing -> do
          throw $ NoIdentifyPage p
        Just p -> do
          pure p

    connect :: (WS.Connection -> m ()) -> WS.Connection -> m ()
    connect run conn = do
      putStrLn "Connect"

      -- Can't figure out how to lift this
      -- WS.withPingThread conn 30 (return ()) $ do
      -- finally disconnect $ forever (run conn)

      forever (run conn)

    onError :: SocketError -> m ()
    onError e = do
      -- putStrLn "ERROR"
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
register :: forall page m. (MonadIO m, MonadBase IO m, MonadBaseControl IO m) => Identified page -> (page -> Encoded 'Model -> [Encoded 'Action] -> m Response) -> MVar (Encoded 'Model) -> WS.Connection -> m ()
register (Identified page encModel) run state conn = do
  putStrLn "Talk"
  msg <- liftIO $ WS.receiveData conn :: m Message
  print msg

  res <- updateState state msg

  let out = Lucid.renderBS (resView res)
  liftIO $ WS.sendTextData conn out

  where



    updateState :: MVar (Encoded 'Model) -> Message -> m Response
    updateState st msg = do
      res <- modifyMVar st updateEnc
      pure res

      where
        -- no type safety yet
        -- grr 
        updateEnc :: Encoded 'Model -> m (Encoded 'Model, Response)
        updateEnc encModel = do
          -- (Runtime.update page)
          let encActions = map Encoded $ fromMessage msg :: [Encoded 'Action]
          res <- run page encModel encActions
          pure (resModel res, res)


          -- pure (res


        -- TODO move to Runtime
        parseModel :: LiveModel model => Encoded 'Model -> m model
        parseModel encModel = do
          case decodeModel encModel of
            Error e -> throw $ InvalidModel (show page) encModel
            Success m -> pure m

        -- parseActions :: Encoded 'Model -> IO []
        -- parseActions encModel = do
        --   case decodeModel encModel of
        --     Error e -> throw $ InvalidModel page encModel
        --     Success m -> pure m

        -- update :: model -> IO model
        -- update m = do
        --   case decodeAction (Encoded $ cs $ fromMessage msg) of
        --     Error _ -> throw $ InvalidAction msg
        --     Success act -> do
        --       m' <- (Runtime.update pg) act m
        --       pure m'



data SocketError
  = NoIdentify Message
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