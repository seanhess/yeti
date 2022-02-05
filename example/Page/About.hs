{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.About where

import Juniper.Web (pageUrl)

-- TODO we need an external "Routes" file so we don't get circular referenes
import Page.Counter as Counter (Params)

import Data.Text (Text)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5




view :: Html ()
view = div_ $ do
  h1_ "About"

  -- I want to reference a certain page, I have the params for it
  -- yeah... and those should match
  let params = (77, Nothing) :: Counter.Params

  -- do URLs manually, but serialize the state
  p_ $ a_ [href_ $ pageUrl "/app/counter" params] "Counter 77"
  p_ $ a_ [href_ "https://google.com"] "Google"



