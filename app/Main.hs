{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String.Conversions (cs)
import Data.Aeson (Value(..), object, (.=))
-- import Network.Wai.Middleware.Cors
import Web.Scotty
import Lucid (renderBS, Html, renderText)
import Lucid.Html5
import Api (view, runtime, Model(..), Message(action, url))
import Runtime (runtime)
import Control.Monad.IO.Class (liftIO)

lucid :: Html a -> ActionM ()
lucid h = do
  setHeader "Content-Type" "text/html"
  raw . renderBS $ h


main :: IO ()
main = do

  scotty 3000 $ do
    -- middleware simpleCors

    get "/" $ do
      setHeader "Content-Type" "text/html"
      file "js/index.html"

    get "/counter/:count" $ do
      -- this is just a load. It may or may not have an action
      m <- body
      c <- param "count" :: ActionM Integer
      runtime (Api.load c) Api.update Api.view m



    post "/counter/:count" $ do
      -- this'll have an action
      -- m <- jsonData
      -- the body as a bytestring
      -- m <- body
      -- r <- liftIO $ Api.runtime m
      -- json r

    get "/:name" $ do
      name <- param "name"
      html $ mconcat ["Hello: ", name]


