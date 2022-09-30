{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Yeti.Server
  ( Render(..)
  , respondWai
  , defaultConfig
  , simpleDocument
  , input'
  , yeti
  ) where

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString.Lazy (ByteString)
import Data.Default (Default, def)
import Lucid (renderBS, Html, toHtml, Term, term)
import Lucid.Html5
import Network.HTTP.Types (status200)
import Network.HTTP.Types.URI (queryToQueryText)
import Network.Wai (ResponseReceived, Middleware)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Yeti.Embed (liveJS)
import Yeti.Encode (Encoded(..))
import Yeti.Page (RoutePage(..), Response(..), PageHandler, pageUrlPath)
import Yeti.Prelude
import Yeti.Sockets (socketApp)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.Wai as Wai


data Render = Render
  { embedJS :: Bool
  , toDoc :: Html () -> Html ()
  }

instance Default Render where
  def = defaultConfig

defaultConfig :: Render
defaultConfig = Render True (simpleDocument "Yeti" "")




-- -- this should be for the page to make sure they match!
-- pageUrl :: ToParams params => String -> params -> Text
-- pageUrl path ps =

--   -- here, escape differently
--   cs path <> "?" <> queryToText (Params.toParams ps)


type EmbedJS = Bool
type DocumentTitle = Text

type ToDocument = Html () -> Html ()

respondWai :: forall page. RoutePage page => Render -> page -> Response -> (Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
respondWai (Render embJS toDoc) pg (Response encModel encParams view) respWai = do

  let content = Lucid.renderBS $ toDoc $ embedContent view

  respWai $ Wai.responseLBS status200 headers content

  where
    headers =
      -- don't need to send them, because the url already results in the correct page
      -- [ ("X-Params", cs $ queryToText encParams)
      [ ("Content-Type", "text/html")
      ]

    stateJSON :: ByteString
    stateJSON = Aeson.encode $ fromEncoded encModel

    embedStateScript :: Html ()
    embedStateScript = 
      script_ [type_ "text/javascript", id_ "yeti-state" ] $ Text.intercalate "\n"
        [ ""
        , "var yetiPage = " <> (cs $ Aeson.encode $ pageUrlPath pg)
        , "var yetiState = " <> fromEncoded encModel
        , ""
        ]

    -- render the root node and embed the javascript
    embedContent :: Html () -> Html ()
    embedContent v = do
      "\n"
      embedStateScript
      "\n"
      div_ [id_ "yeti-root-content"] v
      "\n"
      when embJS $ script_ [type_ "text/javascript"] liveJS

      -- DEBUGGING MODE
      -- script_ [type_ "text/javascript", src_ "/edom/build.js"] ("" :: Text)
      -- script_ [type_ "text/javascript", src_ "/edom/run.js"] ("" :: Text)

    -- vdom :: (ScottyError e, Monad m) => Text -> Html () -> ActionT e m ()
    -- vdom s h = do
    --   Scotty.setHeader "Content-Type" "application/vdom"
    --   Scotty.raw $
    --     cs s <> "\n" <> (Lucid.renderBS h)




-- | Convenience toDocument function to pass to render. Allows you to add stylesheets and javascript easily
simpleDocument :: DocumentTitle -> Html () -> (Html () -> Html ())
simpleDocument t extra content = do
  html_ $ do
    head_ $ do
      title_ (toHtml t)
      meta_ [charset_ "UTF-8"]
      meta_ [httpEquiv_ "Content-Type", content_ "text/html", charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0" ]
      "\n"
      extra

    "\n"
    body_ $ do
      content




input' :: Term arg result => arg -> result
input' = term "input"



-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
yeti :: forall page m. (MonadBase m IO, MonadIO m, MonadBaseControl IO m, RoutePage page) => Render -> PageHandler page m -> Middleware
yeti cfg run = web . sockets
  where
    sockets :: Middleware
    sockets = websocketsOr defaultConnectionOptions (socketApp run)

    web :: Middleware
    web app req resp =
      let mp = routePage (Wai.pathInfo req) :: Maybe page
          qt = queryToQueryText (Wai.queryString req)
      in case mp of
        Nothing -> app req resp
        (Just p) -> do
          res <- liftBase $ run p Nothing qt []
          respondWai cfg p res resp :: IO ResponseReceived

