{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace (traceM)
import Data.String.Conversions (cs)
import Data.Text as Text (stripPrefix, intercalate, Text)
import Data.Text.IO as Text (readFile)
import qualified Data.Text.Lazy as TL (Text)
import Web.Scotty
import Network.Wai (Request, pathInfo, rawPathInfo, Application)
import Lucid (renderBS, Html)
import Lucid.Html5 (html_, head_, script_, src_, body_, type_)
-- import Counter (view, Model(..), load, update, view)
-- import App (resolve)
import Counter (view, update)
import Wookie.Runtime (Message, Response(..), Page(..), runLoad, runAction)
import Wookie.Router (parsePath)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

import Control.Monad.State.Lazy (StateT, execStateT)
import Text.Read (readMaybe)

lucid :: Html a -> ActionM ()
lucid h = do
  setHeader "Content-Type" "text/html"
  raw . renderBS $ h


-- This embeds the javascript into the page
-- How can I tell if they already have it? By the url?
-- Accept encoding!
-- If they ask for Html, give them the whole thing
-- if they ask for Vdom, just give them the one part
reply :: Text -> Html a -> ActionM ()
reply js h = do
  -- Accept-Encoding: gzip
  -- Accept: application/json
  ha <- header "Accept"

  case ha of
    Just "application/vdom" -> lucid h
    _ -> renderWhole h

  where
    renderWhole h' =
      lucid $ html_ $ do
        head_ $ do
          script_ [type_ "text/javascript"] js
        body_ h'


setPageUrl :: TL.Text -> ActionM ()
setPageUrl u =
  setHeader "X-Page-Url" u

-- TODO update url via header
-- TODO handle empty body -> load only


main :: IO ()
main = do

  -- load embedded js
  js <- Text.readFile "js/main.js"

  scotty 3000 $ do
    middleware (delay 100)

    matchAny "/app/counter/:p" $ do
      p <- param "p"
      b <- body
      Response h s <- runAction Counter.update Counter.view p b
      setPageUrl ("/app/counter/" <> cs s)
      reply js h

    get "/:name" $ do
      name <- param "name"
      html $ mconcat ["Hello: ", name]

delay :: Int -> Application -> Application
delay d application req respond = do
  threadDelay (d * 1000)
  application req respond



-- app :: ActionM ()
-- app = do
--   m    <- body
--   path <- param "path" :: ActionM Text

--   -- what if we don't find a page
--   -- liftIO $ print (path, parsePath path)
--   let Just go = App.resolve path
--   Response view <- liftIO $ go m

--   js <- liftIO $ Text.readFile "js/main.js"

--   lucid $ html_ $ do
--     head_ $ do
--       script_ [type_ "text/javascript"] js
--     body_ view




-- appRoutePattern :: Text -> Request -> Maybe [Param]
-- appRoutePattern prefix req = do
--   let totalPath = cs $ rawPathInfo req
--   appPath <- Text.stripPrefix prefix totalPath
--   pure [("path", cs appPath)]
