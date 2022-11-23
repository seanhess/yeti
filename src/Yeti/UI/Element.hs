module Yeti.UI.Element where


import Yeti.Prelude
import Yeti.Events (onClick, onInput)
import Yeti.Encode (LiveAction)
import Yeti.UI.CSS
import Yeti.View





row :: TagMod -> View Content () -> View Content ()
-- row f = el (flex Row . flex () . f)
row f = el (cls [row'] . f)

row_ :: View Content () -> View Content ()
row_ = row id

col :: TagMod -> View Content () -> View Content ()
-- col f = el (flex Col . flex () . f)
col f = el (cls [col'] . f)

col_ :: View Content () -> View Content ()
col_ = col id

space :: View Content ()
space = el grow $ pure ()

button :: LiveAction action => action -> TagMod -> View Content () -> View Content ()
button act f cnt = tag "button" (f . onClick act) cnt

input :: LiveAction action => (Text -> action) -> TagMod -> View Content ()
input act f = tag "input" (f . onInput act) none

hlink :: AttValue -> View a () -> View b ()
hlink href = tag "a" (att "href" href)



pad :: PxRem -> TagMod
pad n = cls [pad' n]

padX :: PxRem -> TagMod
padX n = cls [padX' n]

padY :: PxRem -> TagMod
padY n = cls [padY' n]

gap :: PxRem -> TagMod
gap n = cls [gap' n]

grow :: TagMod
grow = cls [grow']

shadow :: TagMod
shadow = cls [shadow']

bg :: ToColor c => c -> TagMod
bg c = cls [bg' c]

color :: ToColor c => c -> TagMod
color c = cls [color' c]