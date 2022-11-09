{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Yeti.UI.Tailwind.Values where

import Prelude hiding ((-))
import Data.Text as Text (Text, replace, pack, intercalate)
import Yeti.UI.Tailwind.Types


generateValueMap :: [(String, Text)]
generateValueMap = mconcat
  [ map each (range :: [RelSize])
  , map each (range :: [Size])
  , map each (range :: [XSML])
  , map each (range :: [SML])
  , map each (range :: [BorderSize])
  , map each (range :: [Rot])
  , map each (range :: [Dur])
  , map each (range :: [Z])
  , map each (range :: [Opacity])
  ]
  where
    range :: (Bounded a, Enum a) => [a]
    range = [minBound..maxBound]

    each :: (Show a, Segment a) => a -> (String, Text)
    each a = (show a, val a)

    val a = fromSeg $ seg a
    
toJavascript :: [(String, Text)] -> Text
toJavascript ks = "{" <> Text.intercalate ",\n  " (map pair ks) <> "}"
  where
    pair (k, v) = quote (pack k)  <> ":" <> quote v
    quote a = "\"" <> a <> "\""


data Side a
  = T a
  | B a
  | L a
  | R a

instance (Segment a) => Segment (Side a) where
  seg (T a) = "t" - seg a
  seg (B a) = "b" - seg a
  seg (L a) = "l" - seg a
  seg (R a) = "r" - seg a


data Axis a
  = X a
  | Y a

instance (Segment a) => Segment (Axis a) where
  seg (X a) = "x" - seg a
  seg (Y a) = "y" - seg a


data Corner a
  = TL a
  | TR a
  | BL a
  | BR a

-- just use show + mapping?
instance (Segment a) => Segment (Corner a) where
  seg (TL a) = "tl" - seg a
  seg (TR a) = "tr" - seg a
  seg (BL a) = "bl" - seg a
  seg (BR a) = "br" - seg a


data Auto = Auto
  deriving (Enum, Bounded, Show, Segment)

data Full = Full
  deriving (Enum, Bounded, Show, Segment)

data BgSize = Cover | Contain
  deriving (Show, Eq, Bounded, Enum, Segment)

data RelSize
  = R1_2
  | R1_3
  | R2_3
  | R1_4
  | R3_4
  deriving (Bounded, Enum, Show)
instance Segment RelSize where
  seg R1_2   = "1/2"
  seg R1_3   = "1/3"
  seg R2_3   = "2/3"
  seg R1_4   = "1/4"
  seg R3_4   = "3/4"

data ExtSize
  = R1_5
  | R2_5
  | R3_5
  | R4_5
  | R1_6
  | R5_6
  | R1_12
  | R5_12
  | R7_12
  | R11_12
  | Screen
  | Min
  | Max
  deriving (Bounded, Enum, Show)
instance Segment ExtSize where
  seg Screen = "screen"
  seg Min    = "min"
  seg Max    = "max"
  seg s = Seg $ Text.replace "_" "/" $ pack $ drop 1 $ show s
  


data Size
  = Px
  | S0
  | S0_5
  | S1
  | S1_5
  | S2
  | S2_5
  | S3
  | S3_5
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | S12
  | S14
  | S16
  | S20
  | S24
  | S28
  | S32
  | S36
  | S40
  | S44
  | S48
  | S52
  | S56
  | S60
  | S64
  | S72
  | S80
  | S96
  deriving (Eq, Enum, Bounded, Show)
instance Segment Size where
  seg Px = "px"
  seg s = Seg $ Text.replace "_" "." $ pack $ drop 1 $ show s


data XSML
  = Xs
  | Base
  | Xl4
  | Xl5
  | Xl6
  | Xl7
  | Xl8
  | Xl9
  deriving (Enum, Bounded, Show)
instance Segment XSML where
  seg Xs = "xs"
  seg Base = "base"
  -- flip around the number
  seg Xl4 = "4xl"
  seg Xl5 = "5xl"
  seg Xl6 = "6xl"
  seg Xl7 = "7xl"
  seg Xl8 = "8xl"
  seg Xl9 = "9xl"


data SML
  = Sm
  | Md
  | Lg
  | Xl
  | Xl2
  | Xl3
  deriving (Enum, Bounded, Show)
instance Segment SML where
  seg Sm = "sm"
  seg Md = "md"
  seg Lg = "lg"
  seg Xl = "xl"
  seg Xl2 = "2xl"
  seg Xl3 = "3xl"



data BorderSize
  = B0
  | B1
  | B2
  | B4
  | B8
  deriving (Enum, Bounded, Show)
instance Segment BorderSize where
  seg B1 = ""
  seg s = segPrefix s

data None = None
  deriving (Show, Bounded, Enum, Segment)


-- Color needs to be a concrete datatype
-- otherwise you need to specify all your colors for this to work at all


-- TODO What to do here?
newtype Color = Color Text
  deriving (Show)
instance Segment Color where
  seg (Color n) = Seg n

-- data ColorWeight
--   = Cw50
--   | Cw100
--   | Cw200
--   | Cw300
--   | Cw400
--   | Cw500
--   | Cw600
--   | Cw700
--   | Cw800
--   | Cw900
--   deriving (Show, Eq, Segment)


-- Is there any easier way to reuse things? 
-- if these were all different types, it would be easier to read the documentation, that's for sure


data FontWeight
  = Thin
  | Extralight
  | Light
  | Normal
  | Medium
  | Semibold
  | Bold
  | Extrabold
  deriving (Show, Bounded, Enum, Segment)

data AlignSEC
  = Start
  | End
  | Center
  deriving (Bounded, Enum, Show, Segment)

data AlignSB
  = Stretch
  | Baseline
  deriving (Bounded, Enum, Show, Segment)

data AlignBAE
  = Between
  | Around
  | Evenly
  deriving (Bounded, Enum, Show, Segment)


data Direction
  = Row
  | Col
  | RowReverse
  | ColReverse
  deriving (Bounded, Enum, Show, Segment)

data Wrap
  = Wrap
  | Nowrap
  | WrapReverse
  deriving (Bounded, Enum, Show, Segment)


data Pos
  = Static
  | Fixed
  | Absolute
  | Relative
  | Sticky
  deriving (Enum, Bounded, Show, Segment)


data Rot = R0 | R1 | R2 | R3 | R6 | R12 | R45 | R90 | R180
  deriving (Show, Bounded, Enum)
instance Segment Rot where
  seg = segPrefix


data Property
  = All
  | Colors
  | Transform
  | Shadow
  | Opacity
  deriving (Show, Bounded, Enum)
instance Segment Property where
  seg = segHyphens


data Dur
  = D75
  | D100
  | D150
  | D200
  | D300
  | D500
  | D700
  | D1000
  deriving (Show, Bounded, Enum)
instance Segment Dur where
  seg = segPrefix


data Z
  = Z0
  | Z10
  | Z20
  | Z30
  | Z40
  | Z50
  deriving (Show, Bounded, Enum)
instance Segment Z where
  seg = segPrefix




data Opacity
  = O0
  | O5
  | O10
  | O20
  | O25
  | O30
  | O40
  | O50
  | O60
  | O70
  | O75
  | O80
  | O90
  | O100
  deriving (Show, Bounded, Enum)
instance Segment Opacity where
  seg = segPrefix


data Ease
  = Linear
  | InOut
  | Out
  | In
  deriving (Show, Bounded, Enum, Segment)

