{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Juniper.Web
  ( handle
  , Render(..)
  , render
  , static
  , def
  , page
  , response
  , params
  , pageUrl
  , document
  , encodeParams
  , rawParams
  , lucid
  ) where

import Juniper.Prelude
import Juniper.Runtime (Response(..), runAction, runLoad, parseBody, Command)
import Juniper.Page (Page, PageAction)
import Juniper.Params as Params (ToParams(..), HasParams(..))
import Juniper.JS as JS
import Data.Aeson (FromJSON)
import Data.Text.Encoding.Base64 (encodeBase64, decodeBase64)
import Data.Text as Text (intercalate)
import Data.List as List (lookup)
import Web.Scotty (RoutePattern)
import Web.Scotty.Trans (ActionT, ScottyT, ScottyError)
import qualified Web.Scotty.Trans as Scotty
import Lucid (renderBS, Html, toHtml)
import Lucid.Html5
import Network.Wai (rawPathInfo)
import Data.Default (Default, def)

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson

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

instance Default Render where
  def = Render True (document "" "")

-- handle handles it if you're in actionM
handle
  :: forall params model action e m. (FromJSON model, HasParams model params, PageAction action, ToParams params, MonadIO m, ScottyError e)
  => Render -> Page params model action (ActionT e m) -> ActionT e m ()
handle (Render js doc) pg = do
  Response h p' <- response pg
  setParams p'
  render js doc h


response
  :: forall params model action e m. (FromJSON model, PageAction action, HasParams model params, ToParams params, MonadIO m, ScottyError e)
  => Page params model action (ActionT e m) -> ActionT e m (Response params)
response pg = do
  ps <- params
  (mm, cmds) <- parseBody =<< Scotty.body
  case mm of
    Nothing -> runLoad pg ps
    Just m -> runAction pg m cmds

params :: (Monad m, HasParams model params) => ActionT e m params
params = do
  ps <- Scotty.params
  pure $ fromMaybe defParams $ decParams ps

-- parseParams :: HasParams model params => [(Text, Text)] -> params
-- parseParams ps = fromMaybe defParams $ do
--   -- 1 convert to JSON, then parse the json
--   -- 2 default to your parameters
--   Object $ HM.fromList

-- parse params. If we don't have them, use the defaults
-- oh, we aren't parsing them this way any more

-- params :: (Monad m, ScottyError e, TestParams model params) => ActionT e m params
-- params = do
--   -- this may be empty. If it is, then return defaults
--   rps <- rawParams
--   pure $ fromMaybe defaults $ do
--     raw <- rps
--     bps <- decode64 raw
--     ps <- Params.decode bps
--     pure ps
--   where
--     decode64 rw =
--       case (decodeBase64 rw) of
--         -- this should never happen, fail
--         Left e -> fail $ "Could not decode params: Invalid Base64 - " <> cs e
--         Right ps -> Just ps





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
      div_ [id_ "juniper-root-content"] v
      when embJS $ do
        script_ [type_ "text/javascript"] JS.scripts

      -- DEBUGGING MODE
      -- script_ [type_ "text/javascript", src_ "/edom/build.js"] ("" :: Text)
      -- script_ [type_ "text/javascript", src_ "/edom/run.js"] ("" :: Text)



-- | Convenience toDocument function to pass to render. Allows you to add stylesheets and javascript easily
document :: Text -> Html () -> (Html () -> Html ())
document t extra content = do
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


