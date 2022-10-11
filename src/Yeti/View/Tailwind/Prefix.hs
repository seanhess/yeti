{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Yeti.View.Tailwind.Prefix where

import Prelude
import Yeti.View.Tailwind.Types
import Data.Text as Text (Text)
import Data.String (IsString)

newtype Prefix = Prefix { fromPrefix :: Text }
  deriving newtype (IsString)

-- * Prefixes, only apply styles in certain situations
-- | Apply a prefix to classes  
-- > [ bg Green, active |: bg Green ]
addPrefix :: Prefix -> [Class] -> [Class]
addPrefix (Prefix p) cs =
  -- apply cs
  map apply cs
  where
    -- ignore transform
    apply "transform" = "transform"
    apply (Class c') = Class $ p <> ":" <> c'

(|:) :: Prefix -> [Class] -> [Class]
p |: cs = addPrefix p cs

active :: Prefix
active = "active"

hover :: Prefix
hover = "hover"

focus :: Prefix
focus = "focus"

md :: Prefix
md = "md"

