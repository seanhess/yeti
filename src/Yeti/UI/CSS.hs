{-# LANGUAGE OverloadedLabels #-}
module Yeti.UI.CSS where

import Yeti.Prelude hiding ((-))
import Yeti.View.Types
import Yeti.View.Tag
import Text.RawString.QQ (r)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map


-- Px, converted to Rem
type PxRem = Int


(-) :: Show a => Text -> a -> ClassName
n - a = ClassName NoPsd $ n <> "-" <> cs (show a)

cls1 :: Class -> TagMod Class
cls1 c = cls [ c ]

pad :: PxRem -> TagMod Class
pad n =
  cls1 $ Class
    ("pad"-n)
    [("padding", pxRem n)]

padY :: PxRem -> TagMod Class
padY n =
  cls1 $ Class
    ("pady"-n)
    [("padding-top", pxRem n)
    ,("padding-bottom", pxRem n)
    ]

padX :: PxRem -> TagMod Class
padX n =
  cls1 $ Class
    ("padx"-n)
    [("padding-left", pxRem n)
    ,("padding-right", pxRem n)
    ]

gap :: PxRem -> TagMod Class
gap n =
  cls1 $ Class
  ("gap"-n)
  [("gap", pxRem n)]

grow :: TagMod Class
grow =
  cls1 $ Class
    ("grow")
    [("flex-grow", "1")]

flexRow :: TagMod Class
flexRow = cls1 $ Class "row" [("display", "flex"), ("flex-direction", "row")]

flexCol :: TagMod Class
flexCol = cls1 $ Class "col" [("display", "flex"), ("flex-direction", "column")]


shadow :: TagMod Class
shadow = cls1 $ Class "shadow" [("box-shadow", "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)")]



bg :: ToColor c => c -> TagMod Class
bg c =
  cls1 $ Class
    (ClassName NoPsd $ "bg-" <> colorName c)
    [ ("background-color", colorValue c)
    ]

color :: ToColor c => c -> TagMod Class
color c =
  cls1 $ Class
    (ClassName NoPsd $ "clr-" <> colorName c)
    [("color", colorValue c)]


bold :: TagMod Class
bold = cls1 $ Class "bold" [("font-weight", "bold")]


border :: Int -> TagMod Class
border p = cls1 $ Class
  ("border"-p)
  [("border", Style Px (show p))]






hover :: Pseudo
hover = Hover

-- this can work on any tag function?
(|:) :: Pseudo -> TagMod Class -> TagMod Class
(|:) p f t =
  let t' = f t
  in case t'.classes of
    [] -> t'
    (new:cx) ->
      -- this is a bit of a hack
      -- we know that the last function
      t' { classes = (map prefixClass new) : cx }
  where
    prefixClass (Class (ClassName _ n) v) =
      Class (ClassName p n) v

infixr 9 |:



pxRem :: PxRem -> Style
pxRem 0 = Style Px "0"
pxRem 1 = Style Px "1"
pxRem n = Style Rem (show $ fromIntegral n / 16.0)

rgb :: Int -> Int -> Int -> Style
rgb rd gr bl = Style RGB $ mconcat [(show rd), " ", (show gr), " ", (show bl)]


class ToColor a where
  colorValue :: a -> Style
  colorName :: a -> Text