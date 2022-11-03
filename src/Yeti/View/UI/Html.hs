{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Yeti.View.UI.Html where

import Yeti.View.Types
import Yeti.Prelude

script :: p -> View Content () -> View Content ()
script f = tag "script" (att "type" "text/javascript")
script' src = tag "script" (att "type" "text/javascript" . att "src" src) ""

link href = tag "a" (att "href" href)


