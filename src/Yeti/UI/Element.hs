module Yeti.UI.Element where


import Yeti.Prelude
import Yeti.UI.Style
import Yeti.UI.Tailwind.Values
import Yeti.Events (onClick, onInput)
import Yeti.Encode (LiveAction)
import Yeti.View (View, Content, AttMod, AttValue, el, tag, att, none)

row :: AttMod -> View Content () -> View Content ()
row f = el (flex Row . flex () . f)

row_ :: View Content () -> View Content ()
row_ = row id

col :: AttMod -> View Content () -> View Content ()
col f = el (flex Col . flex () . f)

col_ :: View Content () -> View Content ()
col_ = col id

space :: View Content ()
space = el grow $ pure ()

button :: LiveAction action => action -> AttMod -> View Content () -> View Content ()
button act f cnt = tag "button" (f . onClick act) cnt

input :: LiveAction action => (Text -> action) -> AttMod -> View Content ()
input act f = tag "input" (f . onInput act) none

link :: AttValue -> View a () -> View b ()
link href = tag "a" (att "href" href)