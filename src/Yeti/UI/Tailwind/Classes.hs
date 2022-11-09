{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Yeti.UI.Tailwind.Classes where

import Prelude hiding ((-))
import Yeti.UI.Tailwind.Types
import Yeti.UI.Tailwind.Values
import Yeti.UI.Tailwind.Options

bg :: Option Background o => o -> [Class]
bg o = cls $ "bg" - (option o :: Seg Background)

-- We have to create functions for each because padding is the
-- only one that doesn't hyphenate axes and sides
p :: Option Padding o => o -> [Class]
p o = cls $ "p" - (option o :: Seg Padding)

px :: Option Padding o => o -> [Class]
px o = cls $ "px" - (option o :: Seg Padding)

py :: Option Padding o => o -> [Class]
py o = cls $ "py" - (option o :: Seg Padding)

pt :: Option Padding o => o -> [Class]
pt o = cls $ "pt" - (option o :: Seg Padding)

pb :: Option Padding o => o -> [Class]
pb o = cls $ "pb" - (option o :: Seg Padding)

pr :: Option Padding o => o -> [Class]
pr o = cls $ "pr" - (option o :: Seg Padding)

pl :: Option Padding o => o -> [Class]
pl o = cls $ "pl" - (option o :: Seg Padding)

border :: Option Border o => o -> [Class]
border o = cls $ "border" - (option o :: Seg Border)

-- | The distance between child elements
gap :: Option Gap o => o -> [Class]
gap o = cls $ "gap" - (option o :: Seg Gap)

-- | Width and Height
h :: Option Dimensions o => o -> [Class]
h o = cls $ "h" - (option o :: Seg Dimensions)

w :: Option Dimensions o => o -> [Class]
w o = cls $ "w" - (option o :: Seg Dimensions)

flex :: Option Flex o => o -> [Class]
flex opts = cls ("flex"-(option opts :: Seg Flex))

basis :: Option Dimensions o => o -> [Class]
basis o = cls ("basis"-(option o :: Seg Dimensions))

self :: Option Self o => o -> [Class]
self opts = cls $ "self"-(option opts :: Seg Self)

items :: Option Items o => o -> [Class]
items opts = cls $ "items"-(option opts :: Seg Items)

content :: Option Content o => o -> [Class]
content opts = cls $ "content"-(option opts :: Seg Content)

justify :: Option Justify o => o -> [Class]
justify opts = cls $ "justify"-(option opts :: Seg Justify)

-- TODO grow-0?
grow :: [Class]
grow = cls "grow"

shrink :: [Class]
shrink = cls "shrink"


position :: Option Position o => o -> [Class]
position o = cls $ (option o :: Seg Position)

top :: Option Offset o => o -> [Class]
top o = cls $ "top" - (option o :: Seg Offset)

bottom :: Option Offset o => o -> [Class]
bottom o = cls $ "bottom" - (option o :: Seg Offset)

left :: Option Offset o => o -> [Class]
left o = cls $ "left" - (option o :: Seg Offset)

right :: Option Offset o => o -> [Class]
right o = cls $ "right" - (option o :: Seg Offset)

inset :: Option Inset o => o -> [Class]
inset o = cls $ "inset" - (option o :: Seg Inset)

-- | Transforms
-- > (transform, active |: translate (X Px, Y Px))
-- the "transform" property is required
translate :: Option Translate o => o -> [Class]
translate o = cls $ "translate" - (option o :: Seg Translate)

rotate :: Option Rotate o => o -> [Class]
rotate o = cls $ "rotate" - (option o :: Seg Rotate)

transform :: Option Transform o => o -> [Class]
transform o = cls $ "transform" - (option o :: Seg Transform)

transition :: Option Transition o => o -> [Class]
transition o = cls $ "transition" - (option o :: Seg Transition)

duration :: Option Duration o => o -> [Class]
duration o = cls $ "duration" - (option o :: Seg Duration)

easing :: Option Easing o => o -> [Class]
easing o = cls $ "easing" - (option o :: Seg Easing)

delay :: Option Duration o => o -> [Class]
delay o = cls $ "delay" - (option o :: Seg Duration)

rounded :: Option Rounded o => o -> [Class]
rounded o = cls $ "rounded" - (option o :: Seg Rounded)

font :: Option Font o => o -> [Class]
font o = cls $ "font" - (option o :: Seg Font)

text :: Option FontText o => o -> [Class]
text o = cls $ "text" - (option o :: Seg FontText)

outline :: Option Outline o => o -> [Class]
outline o = cls $ "outline" - (option o :: Seg Outline)

shadow :: Option Shadow o => o -> [Class]
shadow o = cls $ "shadow" - (option o :: Seg Shadow)

zIndex :: Option ZIndex o => o -> [Class]
zIndex o = cls $ "z" - (option o :: Seg ZIndex)

opacity :: Option Opacity o => o -> [Class]
opacity o = cls $ "opacity" - (option o :: Seg Opacity)

uppercase :: [Class]
uppercase = ["uppercase"]

lowercase :: [Class]
lowercase = ["lowercase"]

capitalize :: [Class]
capitalize = ["lowercase"]

normalCase :: [Class]
normalCase = ["normal-case"]

underline :: [Class]
underline = ["underline"]

overline :: [Class]
overline = ["overline"]

lineThrough :: [Class]
lineThrough = ["line-through"]

noUnderline :: [Class]
noUnderline = ["no-underline"]

static :: [Class]
static = ["static"]

fixed :: [Class]
fixed = ["fixed"]

absolute :: [Class]
absolute = ["absolute"]

relative :: [Class]
relative = ["relative"]

sticky :: [Class]
sticky = ["sticky"]