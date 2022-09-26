module Sockets where

import Juniper.Prelude
import Lucid

import qualified Juniper.Runtime as Runtime
import Juniper hiding (page)
import Juniper.Encode (Encoded(..))
import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets as WS
import Network.WebSockets (WebSocketsData)
import Control.Exception (finally, Exception, throw, mask, onException, catch, AsyncException)
import Control.Concurrent (forkIO, ThreadId, throwTo, MVar, newMVar, putMVar, takeMVar)
import Control.Monad (forever, forM_)
import Data.Aeson as Aeson
import Data.Aeson.Types
import Web.Scotty.Trans as Scotty

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




newtype Message = Message { fromMessage :: ByteString }
  deriving newtype (Eq, WS.WebSocketsData)

instance Show Message where
  show (Message m) = "|" <> cs m <> "|"



-- we could just have all of this run in their monad?

startLiveView :: forall page m a. (FromJSON page, Show page) => (page -> IO (WS.Connection -> IO ())) -> IO ()
startLiveView pageRun = do

  putStrLn "startLiveView"
  liftIO $ WS.runServer "127.0.0.1" 9160 $ \pending -> do
    putStrLn "Connection!"

    conn <- WS.acceptRequest pending
    page <- identify conn
    run <- pageRun page

    connect run conn `catch` onError

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
          -- WS.sendTextData conn ("Identified" :: Text)
          pure p

    connect :: (WS.Connection -> IO ()) -> WS.Connection -> IO ()
    connect run conn = do
      putStrLn "Connect"
      WS.withPingThread conn 30 (return ()) $ do
        -- finally disconnect $ forever (run conn)
        forever (run conn)

    onError :: SocketError -> IO ()
    onError e = do
      -- putStrLn "ERROR"
      print e

    disconnect :: IO ()
    disconnect = do
      -- perform any cleanup here
      putStrLn "DISCONNECT!"
      pure ()




-- It's all right here, except for the connection
register :: forall action model params. (LiveAction action, Show model) => Page params model action IO -> model -> IO (WS.Connection -> IO ())
register pg initModel = do
  putStrLn "Register"
  st <- liftIO $ newMVar initModel
  putStrLn "Registered"

  pure $ \conn -> do
    putStrLn "Talk"
    msg <- WS.receiveData conn :: IO Message
    print msg

    m <- updateState st msg

    let bs = render m
    WS.sendTextData conn bs

  where

    render :: model -> ByteString
    render m =
      let h = (Runtime.view pg) m
      in Lucid.renderBS h


    updateState :: MVar model -> Message -> IO model
    updateState st msg = do
      modifyMVar st update
      where
        update :: model -> IO model
        update m = do
          case decodeAction (Encoded $ cs $ fromMessage msg) of
            Error _ -> throw $ InvalidAction msg
            Success act -> do
              m' <- (Runtime.update pg) act m
              pure m'



data SocketError
  = NoIdentify Message
  | InvalidAction Message
  deriving (Show, Exception)





-- Taken from Control.Concurrent. Returns the modified variable at the end
modifyMVar :: MVar a -> (a -> IO a) -> IO a
modifyMVar m io =
  mask $ \restore -> do
    a  <- takeMVar m
    a' <- restore (io a) `onException` putMVar m a
    putMVar m a'
    pure a'

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