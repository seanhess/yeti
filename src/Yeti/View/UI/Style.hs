{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Yeti.View.UI.Style where

import Yeti.Prelude
import qualified Yeti.View.Tailwind as Tailwind
import Yeti.View.Types


flex o = cls (Tailwind.flex o)

content o = cls (Tailwind.content o)
self o    = cls (Tailwind.self o)
items o   = cls (Tailwind.items o)
basis o   = cls (Tailwind.basis o)
justify o = cls (Tailwind.justify o)

w o = cls (Tailwind.w o)
h o = cls (Tailwind.h o)

gap o = cls (Tailwind.gap o)

p  o = cls (Tailwind.p o)
px o = cls (Tailwind.px o)
py o = cls (Tailwind.py o)
pl o = cls (Tailwind.pl o)
pr o = cls (Tailwind.pr o)
pt o = cls (Tailwind.pt o)
pb o = cls (Tailwind.pb o)

bg o = cls (Tailwind.bg o)

border o = cls (Tailwind.border o)

text o = cls (Tailwind.text o)
font o = cls (Tailwind.font o)

grow   = cls Tailwind.grow
shrink = cls Tailwind.shrink

uppercase = cls Tailwind.uppercase
lowercase = cls Tailwind.lowercase
normalCase = cls Tailwind.normalCase
capitalize = cls Tailwind.capitalize

underline = cls Tailwind.underline
overline = cls Tailwind.overline
lineThrough = cls Tailwind.lineThrough
noUnderline = cls Tailwind.noUnderline

translate o = cls (Tailwind.translate o)
transform o = cls (Tailwind.transform o)

left o = cls (Tailwind.left o)
right o = cls (Tailwind.right o)
top o = cls (Tailwind.top o)
bottom o = cls (Tailwind.bottom o)

placeholder = att "placeholder"

absolute = cls (Tailwind.absolute)
relative = cls (Tailwind.relative)

