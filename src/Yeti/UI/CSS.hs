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
  ("pad" <> show n)
  [("padding", pxRem n)]

gap' :: PxRem -> Class
gap' n = Class
  ("gap" <> show n)
  [("gap", pxRem n)]

grow' :: Class
grow' = Class
  ("grow")
  [("flex-grow", "1")]

row' :: Class
row' = Class "row" [("display", "flex"), ("flex-direction", "row")]

col' :: Class
col' = Class "col" [("display", "flex"), ("flex-direction", "col")]

pxRem :: PxRem -> ClassValue
pxRem 0 = ClassValue "0" Px
pxRem 1 = ClassValue "1" Px
pxRem n = ClassValue (show $ fromIntegral n / 16.0) Rem






