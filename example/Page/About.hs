{-# LANGUAGE OverloadedStrings #-}
module Page.About where

import Yeti.Web (pageUrl)

-- TODO we need an external "Routes" file so we don't get circular referenes
-- import Page.Counter as Counter (Params)

import Prelude
import Data.Text (Text)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5




view :: Html ()
view = div_ $ do
  h1_ "About"

--   let params = (77, Nothing) :: Counter.Params

--   p_ $ a_ [href_ $ pageUrl "/app/counter" params] "Counter 77"
--   p_ $ a_ [href_ "https://google.com"] "Google"



