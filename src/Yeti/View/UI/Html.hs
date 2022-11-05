{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Yeti.View.UI.Html where

import Yeti.View.Types
import Yeti.Prelude

-- content? or text?
data Scripts
script' :: Text -> View a ()
script' = tag "script" (att "type" "text/javascript") . fromText

none :: View Content ()
none = ""

type Url = Text
script :: Url -> View b ()
script src = tag "script" (att "type" "text/javascript" . att "src" src) none

link href = tag "a" (att "href" href)


