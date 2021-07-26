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
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.String.Conversions (cs)
import Data.Text as Text (Text, intercalate)
import Data.List as List (lookup)
import Web.Scotty (ActionM, ScottyM, RoutePattern)
import qualified Web.Scotty as Scotty
import Lucid (renderBS, Html)
import Lucid.Html5
import Network.Wai (rawPathInfo)

import Text.RawString.QQ



static :: Html () -> ActionM ()
static view =
  -- setPageUrl $ cs path
  lucid view


-- | Scotty Router For a Page
-- page
--   :: forall params model action. (PageAction action, Params params)
--   => String -> Page params model action ActionM -> ScottyM ()
-- page path pg = Scotty.matchAny (Scotty.literal path) $ handlePage path pg


-- Only accepts base paths, don't use params!
page :: RoutePattern -> ActionM () -> ScottyM ()
page = Scotty.matchAny



handle
  :: forall params model action. (PageAction action, ToParams params)
  => (Html () -> Html ()) -> Page params model action ActionM -> ActionM ()
handle doc pg = do
  Response h p' <- response pg
  setParams p'
  render doc h



response
  :: forall params model action. (PageAction action, ToParams params)
  => Page params model action ActionM -> ActionM (Response params)
response pg = do
  ps <- params
  cmds <- commands =<< Scotty.body
  runAction pg ps cmds







params :: ToParams params => ActionM (Maybe params)
params = do
  -- this may be empty. If it is, then return Nothing
  rps <- rawParams
  case rps of
    Nothing -> pure Nothing
    Just raw -> do
      case (decodeBase64 raw) of
        Left e -> fail $ "Could not decode params: " <> cs e
        Right ps ->
          case (Params.decode ps) of
            Nothing -> fail $ "Could not decode params: " <> cs (show rps)
            Just a -> pure $ Just a





setParams :: ToParams params => params -> ActionM ()
setParams ps = do
  req <- Scotty.request
  setPageUrl $ pageUrl (cs $ rawPathInfo req) ps




-- TODO Vdom encoding
-- How can I tell if they already have it? By the url?
-- Accept encoding!
-- If they ask for Html, give them the whole thing
-- if they ask for Vdom, just give them the one part
render :: (Html() -> Html ()) -> Html () -> ActionM ()
render toDocument view = do
  Scotty.header "Accept" >>= \case
    Just "application/vdom" -> lucid view
    _ -> lucid $ toDocument $ embedContent view
  where
    -- render the root node and embed the javascript
    embedContent :: Html () -> Html ()
    embedContent v = do
      div_ [id_ "wookie-root-content"] v
      script_ [type_ "text/javascript"] JS.build
      script_ [type_ "text/javascript"] JS.run

      -- DEBUGGING MODE
      -- script_ [type_ "text/javascript", src_ "/edom/build.js"] ("" :: Text)
      -- script_ [type_ "text/javascript", src_ "/edom/run.js"] ("" :: Text)



-- | Convenience toDocument function to pass to render. Allows you to add stylesheets and javascript easily
document :: Html () -> Html () -> Html ()
document extra content = do
  html_ $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      meta_ [httpEquiv_ "Content-Type", content_ "text/html", charset_ "UTF-8"]

    body_ $ do
      content
      extra

setPageUrl :: Text -> ActionM ()
setPageUrl = Scotty.setHeader "X-Page-Url" . cs


pageUrl :: ToParams params => String -> params -> Text
pageUrl path ps =
  cs path <> "?p=" <> (encodeBase64 $ Params.encode ps)


-- this can return a maybe text
rawParams :: ActionM (Maybe Text)
rawParams = do
  ps <- Scotty.params
  pure $ cs <$> List.lookup "p" ps



lucid :: Html a -> ActionM ()
lucid h = do
  Scotty.setHeader "Content-Type" "text/html"
  Scotty.raw . Lucid.renderBS $ h


