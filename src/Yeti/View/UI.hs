module Yeti.View.UI
  (
  -- * Layout
  row, col, space

  -- * Input
  , button

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
import Yeti.Events (onClick)
import Yeti.Encode (LiveAction)



row :: Att a -> View Content () -> View Content ()
row f = el (flex Row . flex () . f)

col :: Att a -> View Content () -> View Content ()
col f = el (flex Col . flex () . f)

space :: View Content ()
space = el grow $ pure ()

button :: LiveAction action => action -> Att a -> View Content () -> View Content ()
button act f cnt = tag "button" (f . onClick act) cnt
