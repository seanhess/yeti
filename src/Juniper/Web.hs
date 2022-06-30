{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Juniper.Web
  ( handle
  , Render(..)
  , respond
  , static
  , def
  , page
  , pageUrl
  , document
  , lucid
  ) where

import Juniper.Prelude
import Juniper.Runtime as Runtime (Response(..))
import qualified Juniper.Runtime as Runtime
import Juniper.Page (Page, PageAction)
import Juniper.Params as Params (ToParams(..))
import Juniper.State as State (ToState(..))
import Juniper.JS as JS
import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding.Base64 (encodeBase64, decodeBase64)
import Data.Text as Text (intercalate, dropWhile)
import Data.List as List (lookup)
import Web.Scotty (RoutePattern)
import Web.Scotty.Trans (ActionT, ScottyT, ScottyError)
import qualified Web.Scotty.Trans as Scotty
import Lucid (renderBS, Html, toHtml)
import Lucid.Html5
import Network.Wai (rawPathInfo, Request(queryString, rawQueryString))
import Network.HTTP.Types.URI (queryToQueryText, parseQueryText, renderQueryText, QueryText)
import Data.Binary.Builder (toLazyByteString, Builder)
import Data.Default (Default, def)

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson

import Text.RawString.QQ



static :: (ScottyError e, Monad m) => Html () -> ActionT e m ()
static view =
  lucid view


-- Only accepts base paths, don't use params!
page :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
page = Scotty.matchAny

data Render = Render
  { embedJS :: Bool
  , toDoc :: Html () -> Html ()
  }

instance Default Render where
  def = Render True (document "" "")

-- handle handles it if you're in actionM
handle
  :: forall params model action e m. (ToState model, ToParams params, PageAction action, MonadIO m, ScottyError e)
  => Render
  -> Page params model action (ActionT e m)
  -> ActionT e m ()
handle (Render js doc) pg = do
  mps <- Params.decode <$> query
  (mm, cmds) <- Runtime.parseBody =<< Scotty.body

  m <- Runtime.run pg mps mm cmds

  let Response h ps = Runtime.response pg m
  respond js doc ps m h




-- this should be for the page to make sure they match!
pageUrl :: ToParams params => String -> params -> Text
pageUrl path ps =
  cs path <> "?" <> queryToText (Params.encode ps)






-- TODO Vdom encoding
-- How can I tell if they already have it? By the url?
-- Accept encoding!
-- If they ask for Html, give them the whole thing
-- if they ask for Vdom, just give them the one part
-- TODO they should embed the html itself?
-- do we choose how to embed it or not?

respond :: (Monad m, ScottyError e, ToState model, ToParams params) => Bool -> (Html() -> Html ()) -> params -> model -> Html () -> ActionT e m ()
respond embJS toDocument ps model view = do

  setParams

  Scotty.header "Accept" >>= \case
    Just "application/vdom" -> do
      vdom stateString view
      
    _ -> do
      lucid $ toDocument $ embedContent view

  where
    stateString :: Text
    stateString = State.encode model

    stateJSON :: ByteString
    stateJSON = Aeson.encode (cs stateString :: Text)

    setParams = 
      Scotty.setHeader "X-Params" $ cs $ queryToText $ Params.encode ps

    embedStateScript :: Html ()
    embedStateScript = 
      script_ [type_ "text/javascript", id_ "juniper-state" ] ("let juniperState = " <> stateJSON)


    -- render the root node and embed the javascript
    embedContent :: Html () -> Html ()
    embedContent v = do
      embedStateScript
      "\n"
      div_ [id_ "juniper-root-content"] v
      "\n"
      when embJS $ do
        "\n"
        script_ [type_ "text/javascript"] JS.scripts

      -- DEBUGGING MODE
      -- script_ [type_ "text/javascript", src_ "/edom/build.js"] ("" :: Text)
      -- script_ [type_ "text/javascript", src_ "/edom/run.js"] ("" :: Text)

    vdom :: (ScottyError e, Monad m) => Text -> Html () -> ActionT e m ()
    vdom s h = do
      Scotty.setHeader "Content-Type" "application/vdom"
      Scotty.raw $
        cs s <> "\n" <> (Lucid.renderBS h)




-- | Convenience toDocument function to pass to render. Allows you to add stylesheets and javascript easily
document :: Text -> Html () -> (Html () -> Html ())
document t extra content = do
  html_ $ do
    head_ $ do
      title_ (toHtml t)
      meta_ [charset_ "UTF-8"]
      meta_ [httpEquiv_ "Content-Type", content_ "text/html", charset_ "UTF-8"]

    "\n"
    body_ $ do
      "\n"
      content
      "\n"
      extra




query :: (Monad m) => ActionT e m QueryText
query = do
  parseQueryText . rawQueryString <$> Scotty.request

queryToText :: QueryText -> Text
queryToText qt =
  (cs $ toLazyByteString $ renderQueryText False qt)


lucid :: ScottyError e => Monad m => Html a -> ActionT e m ()
lucid h = do
  Scotty.setHeader "Content-Type" "text/html"
  Scotty.raw . Lucid.renderBS $ h



      


