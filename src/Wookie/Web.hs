{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Wookie.Web where


import Wookie.Runtime (Response(..), runAction, commands)
import Wookie.Page (Page, PageAction)
import Wookie.Params as Params (ToParams(..))
import Wookie.JS as JS
import Data.Text.Encoding.Base64 (encodeBase64, decodeBase64)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Function ((&))
import Data.String.Conversions (cs)
import Data.Text as Text (Text, intercalate)
import Data.List as List (lookup)
import Web.Scotty (RoutePattern)
import Web.Scotty.Trans (ActionT, ScottyT, ScottyError)
import qualified Web.Scotty.Trans as Scotty
import Lucid (renderBS, Html, toHtml)
import Lucid.Html5
import Network.Wai (rawPathInfo)
import Control.Monad (when)

import Text.RawString.QQ



static :: (ScottyError e, Monad m) => Html () -> ActionT e m ()
static view =
  lucid view


-- | Scotty Router For a Page
-- page
--   :: forall params model action. (PageAction action, Params params)
--   => String -> Page params model action ActionM -> ScottyM ()
-- page path pg = Scotty.matchAny (Scotty.literal path) $ handlePage path pg


-- Only accepts base paths, don't use params!
page :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
page = Scotty.matchAny

data Render = Render
  { embedJS :: Bool
  , toDoc :: Html () -> Html ()
  }

-- handle handles it if you're in actionM
handle
  :: forall params model action e m. (PageAction action, ToParams params, MonadIO m, ScottyError e)
  => Render -> Page params model action (ActionT e m) -> ActionT e m ()
handle (Render js doc) pg = do
  Response h p' <- response pg
  setParams p'
  render js doc h


response
  :: forall params model action e m. (PageAction action, ToParams params, MonadIO m, ScottyError e)
  => Page params model action (ActionT e m) -> ActionT e m (Response params)
response pg = do
  ps <- params
  cmds <- commands =<< Scotty.body
  runAction pg ps cmds







params :: (Monad m, ScottyError e) => ToParams params => ActionT e m (Maybe params)
params = do
  -- this may be empty. If it is, then return Nothing
  rps <- rawParams
  pure $ do
    raw <- rps
    bps <- decode64 raw
    ps <- Params.decode bps
    pure ps
  where
    decode64 rw =
      case (decodeBase64 rw) of
        -- this should never happen, fail
        Left e -> fail $ "Could not decode params: Invalid Base64 - " <> cs e
        Right ps -> Just ps





setParams :: (ToParams params, Monad m) => params -> ActionT e m ()
setParams ps = do
  Scotty.setHeader "X-Params" $ cs $ encodeParams ps


-- this should be for the page to make sure they match!
pageUrl :: ToParams params => String -> params -> Text
pageUrl path ps =
  cs path <> "?p=" <> encodeParams ps






-- TODO Vdom encoding
-- How can I tell if they already have it? By the url?
-- Accept encoding!
-- If they ask for Html, give them the whole thing
-- if they ask for Vdom, just give them the one part
-- TODO they should embed the html itself?
-- do we choose how to embed it or not?

render :: (Monad m, ScottyError e) => Bool -> (Html() -> Html ()) -> Html () -> ActionT e m ()
render embJS toDocument view = do
  Scotty.header "Accept" >>= \case
    Just "application/vdom" -> do
      lucid view
    _ -> do
      lucid $ toDocument $ embedContent view
  where
    -- render the root node and embed the javascript
    embedContent :: Html () -> Html ()
    embedContent v = do
      div_ [id_ "wookie-root-content"] v
      when embJS $ do
        script_ [type_ "text/javascript"] JS.scripts

      -- DEBUGGING MODE
      -- script_ [type_ "text/javascript", src_ "/edom/build.js"] ("" :: Text)
      -- script_ [type_ "text/javascript", src_ "/edom/run.js"] ("" :: Text)



-- | Convenience toDocument function to pass to render. Allows you to add stylesheets and javascript easily
document :: Text -> Html () -> Render
document t extra = Render True $ \content -> do
  html_ $ do
    head_ $ do
      title_ (toHtml t)
      meta_ [charset_ "UTF-8"]
      meta_ [httpEquiv_ "Content-Type", content_ "text/html", charset_ "UTF-8"]

    body_ $ do
      content
      extra


encodeParams :: ToParams params => params -> Text
encodeParams ps =
  (encodeBase64 $ Params.encode ps)


-- this can return a maybe text
rawParams :: (Monad m, ScottyError e) => ActionT e m (Maybe Text)
rawParams = do
  ps <- Scotty.params
  pure $ cs <$> List.lookup "p" ps



lucid :: ScottyError e => Monad m => Html a -> ActionT e m ()
lucid h = do
  Scotty.setHeader "Content-Type" "text/html"
  Scotty.raw . Lucid.renderBS $ h


