{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Yeti.View.UI where

import Yeti.Prelude
import qualified Yeti.View.Tailwind as Tailwind
import Yeti.View.Types
import Yeti.View.Tailwind.Values (Direction(Row, Col))
-- import Web.UI.Types
-- import Tailwind.Classes
-- import Tailwind.Values
-- import Tailwind.Options
-- import qualified Tailwind.Prefix as Prefix
-- import Tailwind.Types


flex o = addClass (Tailwind.flex o)

content o = addClass (Tailwind.content o)
self o    = addClass (Tailwind.self o)
items o   = addClass (Tailwind.items o)
basis o   = addClass (Tailwind.basis o)
justify o = addClass (Tailwind.justify o)

w o = addClass (Tailwind.w o)
h o = addClass (Tailwind.h o)

gap o = addClass (Tailwind.gap o)

p  o = addClass (Tailwind.p o)
px o = addClass (Tailwind.px o)
py o = addClass (Tailwind.py o)
pl o = addClass (Tailwind.pl o)
pr o = addClass (Tailwind.pr o)
pt o = addClass (Tailwind.pt o)
pb o = addClass (Tailwind.pb o)

bg o = addClass (Tailwind.bg o)

border o = addClass (Tailwind.border o)

text o = addClass (Tailwind.text o)
font o = addClass (Tailwind.font o)

grow   = addClass Tailwind.grow
shrink = addClass Tailwind.shrink

uppercase = addClass Tailwind.uppercase
lowercase = addClass Tailwind.lowercase
normalCase = addClass Tailwind.normalCase
capitalize = addClass Tailwind.capitalize

underline = addClass Tailwind.underline
overline = addClass Tailwind.overline
lineThrough = addClass Tailwind.lineThrough
noUnderline = addClass Tailwind.noUnderline

translate o = addClass (Tailwind.translate o)
transform o = addClass (Tailwind.transform o)

left o = addClass (Tailwind.left o)
right o = addClass (Tailwind.right o)
top o = addClass (Tailwind.top o)
bottom o = addClass (Tailwind.bottom o)

placeholder = setAttribute "placeholder"

absolute' = addClass (Tailwind.absolute)
relative' = addClass (Tailwind.relative)


row :: Att a -> View b () -> View Content ()
row f cnt = el (flex Row . flex () . f) cnt

col :: Att a -> View b () -> View Content ()
col f cnt = el (flex Col . flex () . f) cnt

space :: View Content ()
space = el (grow) $ pure ()
