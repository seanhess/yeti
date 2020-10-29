{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Wookie.Web where


import Wookie.Runtime (Response(..), runAction, command)
import Wookie.Page (Page, PageAction)
import Wookie.Params as Params (Params(..))

import Data.Function ((&))
import Data.String.Conversions (cs)
import Data.Text as Text (Text, intercalate)
import Data.List as List (lookup)
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as Scotty
import Lucid (renderBS, Html)
import Lucid.Html5



static :: String -> Html () -> ScottyM ()
static path view =
  Scotty.get (Scotty.literal path) $ do
    setPageUrl $ cs path
    lucid view


-- | Scotty Router For a Page
page
  :: forall params model action. (PageAction action, Params params)
  => String -> Page params model action ActionM -> ScottyM ()
page path pg = Scotty.matchAny (Scotty.literal path) $ handlePage path pg



handlePage 
  :: forall params model action. (PageAction action, Params params)
  => String -> Page params model action ActionM -> ActionM ()
handlePage path pg = do

  ps <- params

  cmd <- command =<< Scotty.body

  Response h p' <- runAction pg ps cmd

  -- Reply by setting the header and html
  setPageUrl $ pageUrl path p'
  reply h



params :: Params params => ActionM params
params = do

  rps <- rawParams


  -- it can't fail if it's the defaults

  -- if the params are nothing, use the defaults, otherwise, parse them
  let mps = case rps of
            Nothing -> Just $ Params.defaults
            Just ps -> Params.decode ps

  case mps of
    Nothing -> fail $ "Could not decode params: " <> cs (show rps)
    Just a -> pure a




  -- & \case
  --         Nothing -> pure $ Just $ Params.defaults
  --         Just ps -> pure $ Params.decode ps
  -- pure undefined





-- TODO Vdom encoding
-- How can I tell if they already have it? By the url?
-- Accept encoding!
-- If they ask for Html, give them the whole thing
-- if they ask for Vdom, just give them the one part
reply :: Html () -> ActionM ()
reply h = do

  -- Accept-Encoding: gzip
  -- Accept: application/json


  ha <- Scotty.header "Accept"


  case ha of
    Just "application/vdom" -> lucid h
    _ -> renderWhole h

  where
    renderWhole :: Html () -> ActionM ()
    renderWhole h' = do
      lucid $ html_ $ do
        head_ $ do
          meta_ [charset_ "UTF-8"]
          meta_ [httpEquiv_ "Content-Type", content_ "text/html", charset_ "UTF-8"]
          script_ [type_ "text/javascript", src_ "/js/main.js"] ("test()" :: Text)

        body_ $ do
          h1_ "App"
          div_ [id_ "content"] h'




setPageUrl :: Text -> ActionM ()
setPageUrl = Scotty.setHeader "X-Page-Url" . cs


pageUrl :: Params params => String -> params -> Text
pageUrl path ps =
  cs path <> "?p=" <> encode ps


-- this can return a maybe text
rawParams :: ActionM (Maybe Text)
rawParams = do
  ps <- Scotty.params
  pure $ cs <$> List.lookup "p" ps






lucid :: Html a -> ActionM ()
lucid h = do
  Scotty.setHeader "Content-Type" "text/html"
  Scotty.raw . Lucid.renderBS $ h
