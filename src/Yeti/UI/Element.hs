module Yeti.UI.Element where


import Yeti.Prelude
import Yeti.Events (onClick, onInput)
import Yeti.Encode (LiveAction)
import Yeti.UI.CSS
import Yeti.View





row :: TagMod a -> View Content () -> View Content ()
-- row f = el (flex Row . flex () . f)
row f = el (flexRow . f)

row_ :: View Content () -> View Content ()
row_ = row id

col :: TagMod a -> View Content () -> View Content ()
-- col f = el (flex Col . flex () . f)
col f = el (flexCol . f)

col_ :: View Content () -> View Content ()
col_ = col id

space :: View Content ()
space = el grow $ pure ()

button :: LiveAction action => action -> TagMod a -> View Content () -> View Content ()
button act f cnt =
  tag "button" (f . onClick act . att "type" "button" . reset) cnt
  -- include the reset incrementally with UI
  where reset = border 0

input :: LiveAction action => (Text -> action) -> TagMod a -> View Content ()
input act f = tag "input" (f . onInput act) none

hlink :: AttValue -> View a () -> View b ()
hlink href = tag "a" (att "href" href)


