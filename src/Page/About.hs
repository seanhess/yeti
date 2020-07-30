{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.About where


import Data.Text (Text)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5


view :: () -> Html ()
view m = div_ $ do
  h1_ "About"
  p_ $ a_ [href_ "/app/counter/77"] "Counter 77"



