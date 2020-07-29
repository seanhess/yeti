{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String.Conversions (cs)
import Data.Aeson (Value(..), object, (.=))
-- import Network.Wai.Middleware.Cors
import Web.Scotty
import Lucid (renderBS, Html, renderText)
import Lucid.Html5
import Api (view, runtime, Model(..), Message(action, url))
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

    post "/test" $ do
      m <- jsonData
      r <- liftIO $ Api.runtime m
      json r


    get "/:name" $ do
      name <- param "name"
      html $ mconcat ["Hello: ", name]


