{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Yeti.View.UI.Html where

import Yeti.View.Types
-- import Yeti.Prelude

script f = tag "script" (f [attribute "type" "text/javascript"])
script' src = tag "script" [attribute "type" "text/javascript", attribute "src" src] ""

link href = tag "a" [attribute "href" href]


