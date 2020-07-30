{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace (traceM)
import Data.String.Conversions (cs)
import Data.Text as Text (stripPrefix, intercalate, Text)
import Data.Text.IO as Text (readFile)
import Web.Scotty
import Network.Wai (Request, pathInfo, rawPathInfo, Application)
import Lucid (renderBS, Html)
import Lucid.Html5 (html_, head_, script_, src_, body_, type_)
-- import Counter (view, Model(..), load, update, view)
import App (resolve)
import Wookie.Runtime (Message, Response(..))
import Wookie.Router (parsePath)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

lucid :: Html a -> ActionM ()
lucid h = do
  setHeader "Content-Type" "text/html"
  raw . renderBS $ h

-- TODO update url via header
-- TODO handle empty body -> load only


main :: IO ()
main = do

  scotty 3000 $ do
    middleware (delay 1000)

    get (function $ appRoutePattern "/app/") app
    post (function $ appRoutePattern "/app/") app


    get "/:name" $ do
      name <- param "name"
      html $ mconcat ["Hello: ", name]

delay :: Int -> Application -> Application
delay d application req respond = do
  threadDelay (d * 1000)
  application req respond

app :: ActionM ()
app = do
  m    <- body
  path <- param "path" :: ActionM Text

  -- what if we don't find a page
  -- liftIO $ print (path, parsePath path)
  let Just go = App.resolve path
  Response view <- liftIO $ go m

  js <- liftIO $ Text.readFile "js/main.js"

  lucid $ html_ $ do
    head_ $ do
      script_ [type_ "text/javascript"] js
    body_ view


appRoutePattern :: Text -> Request -> Maybe [Param]
appRoutePattern prefix req = do
  let totalPath = cs $ rawPathInfo req
  appPath <- Text.stripPrefix prefix totalPath
  pure [("path", cs appPath)]
