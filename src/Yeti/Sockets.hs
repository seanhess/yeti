module Yeti.Sockets where

import Yeti.Prelude

import Control.Concurrent.MVar.Lifted (MVar, newMVar, modifyMVar)
import Control.Exception.Lifted (Exception, throw, catch)
import Control.Monad (forever)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as Aeson
import Network.WebSockets (WebSocketsData)
import Yeti.Encode (Encoded(..), Encoding(..))
import Yeti.Page (Response(..), PageHandler, RoutePage(..), pathSegments)
import Yeti.View.Types (vdom, viewClasses, ClassName, ClassProps)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import qualified Yeti.Params as Params
import qualified Yeti.Runtime as Runtime



newtype Message = Message { fromMessage :: Text }
  deriving newtype (Eq, Show, WebSocketsData)

data ClientState = ClientState
  { model :: Encoded 'Model
  , classes :: Map ClassName ClassProps
  }


socketApp
  :: forall page m. (RoutePage page, MonadBase m IO, MonadIO m, MonadBaseControl IO m)
  => PageHandler page m
  -> WS.PendingConnection
  -> IO ()
socketApp pageResponse pending = do
  putStrLn "New Connection"

  liftIO $ flip catch onSocketError $ do
    conn <- liftIO $ WS.acceptRequest pending
    cid <- identify conn
    run <- pageRun cid
    connect run conn

  where

    pageRun :: Identified page -> IO (WS.Connection -> IO ())
    pageRun i = do
      var <- newMVar $ ClientState i.idModel []
      pure $ \conn -> (liftBase $ talk i var pageResponse conn) `catch` onRuntimeError
      

    -- this needs to return page and state
    identify :: WS.Connection -> IO (Identified page)
    identify conn = do
      putStrLn " - Waiting for Identify"
      msg <- WS.receiveData conn
      cid <- parseIdentify $ Text.splitOn "\n" $ fromMessage msg
      putStrLn " - Identified"
      pure cid

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
      case routePage (pathSegments p) of
        Nothing -> do
          throw $ NoIdentifyPage p
        Just pg -> do
          pure pg

    connect :: (WS.Connection -> IO ()) -> WS.Connection -> IO ()
    connect run conn = do
      putStrLn " - connect"

      WS.withPingThread conn 30 (return ()) $ do
        -- finally disconnect $ forever (run conn)
        forever (run conn)


    onSocketError :: SocketError -> IO ()
    onSocketError e = do
      putStrLn $ "SOCKET ERROR! " <> show e

    onRuntimeError :: Runtime.Error -> IO ()
    onRuntimeError e = do
      putStrLn $ "RUNTIME ERROR! " <> show e

    -- disconnect :: IO ()
    -- disconnect = do
    --   -- perform any cleanup here
    --   putStrLn "DISCONNECT!"
    --   pure ()


data Identified page = Identified
  { idPage :: page
  , idModel :: Encoded 'Model
  }

-- It's all right here, except for the connection
-- run :: (MonadFail m, MonadIO m) => AppPage -> Encoded 'Encode.Model -> [Encoded 'Encode.Action] -> m Response
talk
  :: forall page m. (MonadIO m, MonadBase m IO, MonadBaseControl IO m)
  => Identified page
  -> MVar ClientState
  -> PageHandler page m
  -> WS.Connection ->
  m ()
talk (Identified page _) state run conn = do
  -- putStrLn $ "TALK: " <> (show page)
  msg <- liftIO $ WS.receiveData conn :: m Message
  -- putStrLn $ " - " <> cs (fromMessage msg)

  (res, newCls) <- updateState state msg

  -- FORMAT
  -- state
  -- html
  liftIO $ WS.sendTextData conn $ Text.intercalate "\n"
    [ fromEncoded $ resModel res
    , Params.queryToText $ resParams res
    , cs $ Aeson.encode $ vdom (resView res)
    , cs $ Aeson.encode $ newCls
    ]

  where

    updateState :: MVar ClientState -> Message -> m (Response, Map ClassName ClassProps)
    updateState st msg = do
      res <- modifyMVar st updateEnc
      pure res

      where
        updateEnc :: ClientState -> m (ClientState, (Response, Map ClassName ClassProps))
        updateEnc em = do
          let als = Text.splitOn "\n" $ fromMessage msg :: [Text]
          let encActions = map Encoded als :: [Encoded 'Action]
          r <- run page (Just em.model) [] encActions

          let cls = viewClasses r.resView
          let allCls = Map.union em.classes cls
          let diffCls = Map.difference cls em.classes

          pure (em { model = resModel r, classes = allCls }, (r, diffCls))





data SocketError
  = NoIdentify [Text]
  | NoIdentifyPage Text
  deriving (Show, Exception)



