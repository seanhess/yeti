{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.About where


import Data.Text (Text)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5




view :: Html ()
view = div_ $ do
  h1_ "About"

  -- I want to reference a certain page, I have the params for it
  -- yeah... and those should match
  p_ $ a_ [href_ "/app/counter?count=77"] "Counter 77"



