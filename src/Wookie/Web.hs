{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Wookie.Web where


import Wookie.Runtime (Page, PageAction, Response(..), runAction, command)
import Wookie.Params (Params(..))

import Data.Function ((&))
import Data.String.Conversions (cs)
import Data.Text as Text (Text, intercalate)
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as Scotty
import Lucid (renderBS, Html)
import Lucid.Html5



-- | Scotty Router For a Page
page
  :: forall params model action. (PageAction action, Params params)
  => String -> Page params model action ActionM -> ScottyM ()
page path pg = Scotty.matchAny (Scotty.literal path) $ handlePage path pg



handlePage 
  :: forall params model action. (PageAction action, Params params)
  => String -> Page params model action ActionM -> ActionM ()
handlePage path pg = do

  qs <- rawParams

  (ps :: params) <- decode qs & \case
          Nothing -> fail $ "Could not decode params: " <> cs qs
          Just a -> pure a

  cmd <- command =<< Scotty.body

  Response h p' <- runAction pg ps cmd

  -- Reply by setting the header and html
  setPageUrl $ pageUrl path p'
  reply h




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
          script_ [type_ "text/javascript", src_ "/js/main.js"] ("test()" :: Text)
        body_ $ do
          h1_ "App"
          div_ [id_ "content"] h'





setPageUrl :: Text -> ActionM ()
setPageUrl = Scotty.setHeader "X-Page-Url" . cs


pageUrl :: Params params => String -> params -> Text
pageUrl path ps =
  cs path <> "?p=" <> encode ps


rawParams :: ActionM Text
rawParams = Scotty.param "p"






lucid :: Html a -> ActionM ()
lucid h = do
  Scotty.setHeader "Content-Type" "text/html"
  Scotty.raw . Lucid.renderBS $ h
