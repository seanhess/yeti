{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Yeti.View.UI
  (
  -- * Layout
  row, col, space

  -- * Classes
  , module Yeti.View.UI.Style

  -- * Values
  , module Yeti.View.Tailwind.Values

  -- * Html
  , module Yeti.View.UI.Html

  -- * Types
  , module Yeti.View.Types

  ) where


import Yeti.Prelude
import Yeti.View.UI.Style
import Yeti.View.Types
import Yeti.View.Tailwind.Values
import Yeti.View.UI.Html



row :: Att a -> View Content () -> View Content ()
row f cnt = el (flex Row . flex () . f) cnt

col :: Att a -> View Content () -> View Content ()
col f cnt = el (flex Col . flex () . f) cnt

space :: View Content ()
space = el (grow) $ pure ()
