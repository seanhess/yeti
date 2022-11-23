{-# LANGUAGE OverloadedLabels #-}
module Yeti.UI.CSS where

import Yeti.Prelude
import Yeti.View.Types
import Text.RawString.QQ (r)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map


-- Px, converted to Rem
type PxRem = Int

pad' :: PxRem -> Class
pad' n = Class
  (ClassName NoPsd $ "pad-" <> cs (show n))
  [("padding", pxRem n)]

padY' :: PxRem -> Class
padY' n = Class
  (ClassName NoPsd $ "pady-" <> cs (show n))
  [("padding-top", pxRem n)
  ,("padding-bottom", pxRem n)
  ]

padX' :: PxRem -> Class
padX' n = Class
  (ClassName NoPsd $ "padx-" <> cs (show n))
  [("padding-left", pxRem n)
  ,("padding-right", pxRem n)
  ]

gap' :: PxRem -> Class
gap' n = Class
  (ClassName NoPsd $ "gap-" <> cs (show n))
  [("gap", pxRem n)]

grow' :: Class
grow' = Class
  ("grow")
  [("flex-grow", "1")]

row' :: Class
row' = Class "row" [("display", "flex"), ("flex-direction", "row")]

col' :: Class
col' = Class "col" [("display", "flex"), ("flex-direction", "column")]

pxRem :: PxRem -> Style
pxRem 0 = Style Px "0"
pxRem 1 = Style Px "1"
pxRem n = Style Rem (show $ fromIntegral n / 16.0)

shadow' :: Class
shadow' = Class "shadow" [("box-shadow", "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)")]


class ToColor a where
  colorValue :: a -> Style
  colorName :: a -> Text

bg' :: ToColor c => c -> Class
bg' c =
  Class
    (ClassName NoPsd $ "bg-" <> colorName c)
    [ ("background-color", colorValue c)
    ]

rgb :: Int -> Int -> Int -> Style
rgb rd gr bl = Style RGB $ mconcat [(show rd), " ", (show gr), " ", (show bl)]



hover :: Pseudo
hover = Hover

-- this is a little baby function that ONLY applies the class
(|:) :: Pseudo -> (Tag -> Tag) -> (Tag -> Tag)
(|:) p f t =
  let t' = f t
  in case t'.classes of
    [] -> t'
    (new:cx) ->
      t' { classes = (map prefixClass new) : cx }
  where
    prefixClass (Class (ClassName _ n) v) =
      Class (ClassName p n) v

infixr 9 |:


