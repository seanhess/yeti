{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Yeti.View.UI.Html where

import Yeti.View.Types
-- import Yeti.Prelude

script f = tag "script" (f [attribute "type" "text/javascript"])


