{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Yeti.View.Tailwind.Options where

import Yeti.View.Tailwind.Values
import Yeti.View.Tailwind.Types

data Background
instance Option Background BgSize
instance Option Background Color
instance Option Background Auto

data Padding
instance Option Padding Size

data Border
instance Option Border BorderSize
instance Option Border (Side BorderSize)
instance Option Border (Axis BorderSize)
instance Option Border Color
instance Option Border (Side Color)
instance Option Border (Axis Color)

data Gap
instance Option Gap Size
instance Option Gap (Axis Size)

data Dimensions
instance Option Dimensions Auto
instance Option Dimensions Full
instance Option Dimensions Size
instance Option Dimensions RelSize
instance Option Dimensions ExtSize

data Flex
instance Option Flex Direction
instance Option Flex Wrap
instance Option Flex ()

data Self
instance Option Self Auto
instance Option Self AlignSEC
instance Option Self AlignSB

data Items
instance Option Items AlignSEC
instance Option Items AlignSB

data Content
instance Option Content AlignSEC
instance Option Content AlignBAE

data Justify
instance Option Justify AlignSEC
instance Option Justify AlignBAE

data Position
instance Option Position Pos

data Offset
instance Option Offset Size
instance Option Offset (Axis Size)

data Inset
instance Option Inset Size
instance Option Inset (Axis Size)

data Translate
instance Option Translate (Axis Size)
instance Option Translate (Axis RelSize)

data Rotate
instance Option Rotate Rot

data Transform
instance Option Transform None

data Transition
instance Option Transition ()
instance Option Transition Property
instance Option Transition None

data Duration
instance Option Duration Dur

data Easing
instance Option Easing Ease

data Rounded
instance Option Rounded ()
instance Option Rounded None
instance Option Rounded Full
instance Option Rounded SML
instance Option Rounded (Side None)
instance Option Rounded (Side Full)
instance Option Rounded (Side SML)
instance Option Rounded (Side ())
instance Option Rounded (Corner None)
instance Option Rounded (Corner Full)
instance Option Rounded (Corner SML)
instance Option Rounded (Corner ())

data FontText
instance Option FontText SML
instance Option FontText XSML
instance Option FontText Color

data Font
instance Option Font FontWeight

data Outline
instance Option Outline None

data Shadow
instance Option Shadow None
instance Option Shadow ()
instance Option Shadow SML

data ZIndex
instance Option ZIndex Z
instance Option ZIndex Auto

instance Option Opacity Opacity