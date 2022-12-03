{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Yeti.Server
  ( Render
  , simpleDocument
  , document
  , respondWai
  , yeti
  , def
  , embedLiveJS
  ) where

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.URI (queryToQueryText)
import Network.Wai (ResponseReceived, Middleware)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Yeti.Embed (liveJS)
import Yeti.Encode (Encoded(..))
import Yeti.Page (RoutePage(..), Response(..), PageHandler, pageUrlPath)
import Yeti.Prelude
import Yeti.View
import Yeti.Sockets (socketApp)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.Wai as Wai

type DocumentTitle = Text
type Render = View Content () -> View Document ()

embedLiveJS :: View Script ()
embedLiveJS = 
  script $ Code $ cs liveJS

def :: View Script ()
def = embedLiveJS

simpleDocument :: DocumentTitle -> View Content () -> View Document ()
simpleDocument title = document title (embedLiveJS)

document :: DocumentTitle -> View Script () -> View Content () -> View Document ()
document title (View scripts) body = View $ runView $
  html_ $ do
    head_ $ do
      title_ title
      meta (charset "UTF-8")
      meta (httpEquiv "Content-Type" . content "text/html" . charset "UTF-8")
      meta (name "viewport" . content "width=device-width, initial-scale=1.0")

    body_ $ do
      body

      -- put scripts last in case they expect things to be loaded
      (View scripts)

  where
    charset = att "charset"
    httpEquiv = att "httpEquiv"
    content = att "content"
    name = att "name"

-- extra :: View Content () -> View Content () -> View Content ()
-- extra a b = a >> b




-- -- this should be for the page to make sure they match!
-- pageUrl :: ToParams params => String -> params -> Text
-- pageUrl path ps =

--   -- here, escape differently
--   cs path <> "?" <> queryToText (Params.toParams ps)



respondWai :: forall page. RoutePage page => Render -> page -> Response -> (Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
respondWai render pg (Response encModel _ view) respWai = do

  let htmlContent = htmlDocument $ render $ embedContent view

  respWai $ Wai.responseLBS status200 headers (cs htmlContent)

  where
    headers =
      -- don't need to send them, because the url already results in the correct page
      -- [ ("X-Params", cs $ queryToText encParams)
      [ ("Content-Type", "text/html")
      ]

    embedStateScript :: View a ()
    embedStateScript = 
      script $ Code $ Text.intercalate "\n"
        [ ""
        , "var YETI_PAGE = " <> (cs $ Aeson.encode $ pageUrlPath pg)
        , "var YETI_STATE = " <> fromEncoded encModel
        , ""
        ]

    embedStyleSheet :: View Content () -> View Content ()
    embedStyleSheet v = do
      let cx = viewClasses v
      tag "style" (att "type" "text/css" . att "id" "yeti-stylesheet") $
        (fromText $ Text.intercalate "\n" $ renderCSS cx)


    -- render the root node and embed the javascript
    embedContent :: View Content () -> View Content ()
    embedContent v = do
      fromText "\n"
      embedStateScript
      fromText "\n"
      embedStyleSheet v
      fromText "\n"
      el (att "id" "yeti-root-content") v

      -- DEBUGGING MODE
      -- script_ [type_ "text/javascript", src_ "/edom/build.js"] ("" :: Text)
      -- script_ [type_ "text/javascript", src_ "/edom/run.js"] ("" :: Text)

    -- vdom :: (ScottyError e, Monad m) => Text -> Html () -> ActionT e m ()
    -- vdom s h = do
    --   Scotty.setHeader "Content-Type" "application/vdom"
    --   Scotty.raw $
    --     cs s <> "\n" <> (Lucid.renderBS h)






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

