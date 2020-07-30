{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace (traceM)
import Data.String.Conversions (cs)
import Data.Text as Text (stripPrefix, intercalate, Text)
import Web.Scotty
import Network.Wai (Request, pathInfo, rawPathInfo)
import Lucid (renderBS, Html)
import Lucid.Html5 (html_, head_, script_, src_, body_, type_)
-- import Counter (view, Model(..), load, update, view)
import App (resolve)
import Wookie.Runtime (runtime, Message, Response(..))
import Wookie.Router (parsePath)
import Control.Monad.IO.Class (liftIO)

lucid :: Html a -> ActionM ()
lucid h = do
  setHeader "Content-Type" "text/html"
  raw . renderBS $ h


main :: IO ()
main = do

  scotty 3000 $ do
    -- middleware simpleCors

    -- get "/" $ do
    --   setHeader "Content-Type" "text/html"
    --   file "js/index.html"

    get "/js/main.js" $ do
      setHeader "Content-Type" "text/javascript"
      file "js/main.js"

    get (function $ appRoutePattern "/app/") app
    post (function $ appRoutePattern "/app/") app


    get "/:name" $ do
      name <- param "name"
      html $ mconcat ["Hello: ", name]


  where

    app :: ActionM ()
    app = do
      m    <- body
      path <- param "path" :: ActionM Text

      -- what if we don't find a page
      liftIO $ print (path, parsePath path)
      let Just go = App.resolve path
      Response view <- liftIO $ go m

      lucid $ html_ $ do
        head_ $ do
          script_ [type_ "text/javascript", src_ "/js/main.js"] ("test()" :: Text)
        body_ view

    appRoutePattern :: Text -> Request -> Maybe [Param]
    appRoutePattern prefix req = do
      let totalPath = cs $ rawPathInfo req
      appPath <- Text.stripPrefix prefix totalPath
      pure [("path", cs appPath)]
